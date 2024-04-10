{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import DA.Auth
import DA.WebSockets
import Data.Function
import Effectful
import Effectful.Concurrent.Async
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Servant.Generic
import Env
import Gogol (newEnv)
import Google (mainProcess)
import Log.Backend.StandardOutput
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import PyF
import URI.ByteString
import UnliftIO.STM
import Dhall

main :: IO ()
main = do
  config <- input auto "./config.dhall"
  googleEnv <- Gogol.newEnv @GoogleScopes
  clientAccessToken <- newEmptyTMVarIO
  donationAlertsChan <- newTQueueIO
  let env = Env {..}
      warpSettings = defaultSettings & setPort 12354

  runEff
    . runConcurrent
    . runReader env
    . runDAOauth2
    . withJsonStdOutLogger
    $ \logger ->
      runLog "da-google-tabs" logger LogTrace do
        authUrl <- authorizeUrl
        logInfo_ [fmt|{serializeURIRef' authUrl}|]
        _ <-
          runConcurrently $
            Concurrently (runWarpServerSettings warpSettings oauthRoutesHandle Data.Function.id)
              *> Concurrently runDAListener
              *> Concurrently mainProcess
        pure ()
