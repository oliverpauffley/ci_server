module Core where

import qualified Codec.Serialise       as Serialise
import qualified Data.Aeson            as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import           GHC.Records           (getField)
import           GHC.Records.Compat    (setField)
import           RIO
import qualified RIO.List              as List
import qualified RIO.Map               as Map
import qualified RIO.NonEmpty          as NonEmpty
import qualified RIO.Text              as Text

data Pipeline
  = Pipeline { steps :: NonEmpty Step }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Step =
  Step
  { name     :: StepName
  , commands :: NonEmpty Text
  , image    :: Docker.Image
  }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

data Build =
  Build
  {
    pipeline       :: Pipeline
  , state          :: BuildState
  , completedSteps :: Map StepName StepResult
  , volume         :: Docker.Volume
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  case allSucceeded of
    True -> case nextStep of
            Just step -> Right step
            Nothing   -> Left BuildSucceeded
    False -> Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps

data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded
  deriving (Eq, Show, Generic, Serialise.Serialise)


data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildRunningState
  = BuildRunningState
    { step      :: StepName
    , container :: Docker.ContainerId
    }
  deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

data Log = Log
  { output :: ByteString
  , step   :: StepName
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int
  deriving (Eq, Show, Ord, Generic, Serialise.Serialise)

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit = case Docker.exitCodeToInt exit of
  0 -> StepSucceeded
  _ -> StepFailed exit

stepNameToText :: StepName -> Text
stepNameToText (StepName step ) = step


progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady      ->
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}
        Right step -> do
          let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands
          let options = Docker.CreateContainerOptions { image = step.image
                , script = script
                , volume = build.volume
                }
          docker.pullImage step.image
          container <- docker.createContainer options
          docker.startContainer container

          let s = BuildRunningState
                { step = step.name
                , container = container
                }
          pure $ build{state = BuildRunning s}

    BuildRunning (BuildRunningState step container)   -> do
      status <- docker.containerStatus container

      case status of
        Docker.ContainerRunning ->
          -- if the container if running we should wait for it to finish
          pure build

        Docker.ContainerExited exit -> do
          let result = exitCodeToStepResult exit
          pure build
            { state = BuildReady
            , completedSteps = Map.insert step result build.completedSteps
            }

        Docker.ContainerOther other -> do
          let s = BuildUnexpectedState other
          pure build {state = BuildFinished s}
    BuildFinished _ -> pure build


collectLogs :: Docker.Service
            -> LogCollection
            -> Build
            -> IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  pure (newCollection, logs)


initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)

updateCollection
  :: BuildState
  -> Time.POSIXTime
  -> LogCollection
  -> LogCollection
updateCollection state lastCollection collection =
  Map.mapWithKey f collection
  where
    update step since nextState =
      case state of
        BuildRunning state ->
          case state.step == step of
            True  -> CollectingLogs state.container since
            False -> nextState
        _ -> nextState
    f step = \case
      CollectionReady ->
        update step 0 CollectionReady
      CollectingLogs _ _ ->
        update step lastCollection CollectionFinished
      CollectionFinished -> CollectionFinished

runCollection
  :: Docker.Service
  -> Time.POSIXTime
  -> LogCollection
  -> IO [Log]
runCollection docker collectUntil collection = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogsOptions
                { container = container
                , since = since
                , until = collectUntil
                }
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]
