module Main where

import           Agent
import qualified Control.Concurrent.Async as Async
import           Core
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.KeyMap        as Aeson.KeyMap
import qualified Data.Yaml                as Yaml
import qualified Docker
import           JobHandler
import qualified JobHandler
import           JobHandler.Memory
import qualified Network.HTTP.Simple      as HTTP
import           RIO
import qualified RIO.ByteString           as ByteString
import qualified RIO.HashMap              as HashMap
import qualified RIO.Map                  as Map
import qualified RIO.NonEmpty.Partial     as P
import qualified RIO.Set                  as Set
import qualified Runner
import           Server
import qualified System.Process.Typed     as Process
import           Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
  { name = StepName name
  , image = Docker.Image { name = image, tag = "latest" }
  , commands = P.fromList commands
  }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = P.fromList steps }

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure ()
    , buildUpdated = \_ -> pure ()
    }



testRunSuccess :: Runner.Service -> IO ()
testRunSuccess  runner = do
  build <- runner.prepareBuild $ makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure  runner = do
  build <- runner.prepareBuild $ makePipeline
    [ makeStep "Should fail" "ubuntu" ["exit 1"]
    ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  build <- runner.prepareBuild $ makePipeline
    [ makeStep "Create file" "ubuntu" ["echo hello > test"]
    , makeStep "Read file" "ubuntu" ["cat test"]
    ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]

  let onLog log = do
          remaining <- readMVar expected
          forM_ remaining $ \word -> do
            case ByteString.breakSubstring word log.output of
              (_, "") -> pure () -- Not found
              _       -> modifyMVar_ expected (pure . Set.delete word)
          pure ()

  let hooks = Runner.Hooks { logCollected = onLog, buildUpdated = \_ -> pure () }

  build <- runner.prepareBuild $ makePipeline
    [ makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
    , makeStep "Echo Linux" "ubuntu" ["uname -s"]
    ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "/run/current-system/sw/bin/docker rmi -f busybox"

  build <- runner.prepareBuild $ makePipeline
    [ makeStep "First step" "busybox" ["date"]
    ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yaml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent =
  runServerAndAgent $ \handler -> do
    let pipeline = makePipeline [ makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]]

    let info =
          JobHandler.CommitInfo
            { sha = "00000"
            , branch = "master"
            , message = "test commit"
            , author = "quad"
            , repo= "quad-ci/quad"
            }

    number <- handler.queueJob info pipeline
    checkBuild handler number
    pure ()

runServerAndAgent :: (JobHandler.Service -> IO()) -> Runner.Service -> IO ()
runServerAndAgent callback runner = do
  handler <- JobHandler.Memory.createService

  serverThread <- Async.async do
    Server.run (Server.Config 9000) handler

  Async.link serverThread

  agentThread <-  Async.async do
    Agent.run (Agent.Config "http://localhost:9000") runner

  Async.link agentThread
  callback handler

  Async.cancel serverThread
  Async.cancel agentThread

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = loop
  where
    loop = do
      Just job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            BuildFinished s -> s `shouldBe` BuildSucceeded
            _               -> loop
        _ -> loop

testWebhookTrigger :: Runner.Service -> IO ()
testWebhookTrigger =
  runServerAndAgent $ \handler -> do
    base <- HTTP.parseRequest "http://localhost:9000"

    let req =
          base
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestPath "/webhook/github"
            & HTTP.setRequestBodyFile "test/github-payload.sample.json"

    res <- HTTP.httpBS req

    let Right (Aeson.Object build) = traceShow res Aeson.eitherDecodeStrict $ HTTP.getResponseBody res
    let Just (Aeson.Number number) = Aeson.KeyMap.lookup "number" build


    checkBuild handler $ Core.BuildNumber (round number)



main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share workspace between steps" do
      testSharedWorkspace runner
    it "should collect logs" do
      testLogCollection runner
    it "should pull images" do
      testImagePull runner
    it "should decode pipelines" do
      testYamlDecoding runner
    it "should run server and agent" do
      testServerAndAgent runner
    it "should process webhooks" do
      testWebhookTrigger runner

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "/run/current-system/sw/bin/docker rm -f $(/run/current-system/sw/bin/docker ps -aq --filter \"label=quad\")"
  Process.readProcessStdout "/run/current-system/sw/bin/docker volume rm -f $(/run/current-system/sw/bin/docker volume ls -q --filter \"label=quad\")"
