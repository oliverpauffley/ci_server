module Github where


import           Core
import           Data.Aeson           ((.:))
import           RIO

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson.Types

import qualified Data.Yaml            as Yaml
import qualified Docker
import qualified JobHandler
import qualified Network.HTTP.Simple  as HTTP
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Text             as Text


parsePushEvent :: ByteString -> IO JobHandler.CommitInfo
parsePushEvent body = do
  let parser = Aeson.withObject "github-webhook" $ \event -> do
        branch <- event .: "ref" <&> \ref -> Text.dropPrefix "refs/head/" ref
        commit <- event .: "head_commit"
        sha <- commit .: "id"
        repo <- event .: "repository" >>= \r -> r .: "full_name"
        message <- commit .: "message"
        author <- commit .: "author" >>= \a -> a .: "username"

        pure JobHandler.CommitInfo
          { sha = sha
          , repo = repo
          , branch = branch
          , message = message
          , author = author
          }

  let result = do
         value <- Aeson.eitherDecodeStrict body
         Aeson.Types.parseEither parser value

  case result of
    Left e     -> throwString e
    Right info -> pure info


fetchRemotePipeline :: JobHandler.CommitInfo -> IO Pipeline
fetchRemotePipeline info = do
  endpoint <- HTTP.parseRequest "https://api.github.com"
  let path = "/repos/" <> info.repo <> "/contents/.quad.yml"

  let req =
        endpoint
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.addToRequestQueryString [("ref", Just $ encodeUtf8 info.sha)]
          & HTTP.addRequestHeader "User-Agent" "quad-ci"
          & HTTP.addRequestHeader "Accept" "application/vnd.github.v3.raw"

  res <- HTTP.httpBS req
  Yaml.decodeThrow $ HTTP.getResponseBody res

createCloneStep :: JobHandler.CommitInfo -> Step
createCloneStep info = Step
  { name = StepName "clone"
  , commands = NonEmpty.Partial.fromList
    [ "git clone -q https://github.com/" <> info.repo <> " ."
    , "git checkout -qf " <> info.sha
    ]
  , image = Docker.Image "alpine/git" "v2.26.2"
  }
