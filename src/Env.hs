module Env where

import DA.WebSockets.Types
import Data.Text
import Effectful.Concurrent.STM (TMVar, TQueue)
import Gogol qualified
import Network.OAuth.OAuth2
import Dhall

type GoogleScopes = '["https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/youtube"]

type GoogleEnv = Gogol.Env GoogleScopes

data Config = Config
  { tableName :: Text,
    tableID :: Text,
    oauth2ClientId :: Text,
    oauth2ClientSecret :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromDhall)

data Env = Env
  { clientAccessToken :: TMVar OAuth2Token,
    donationAlertsChan :: TQueue NewDonationAlertMsg,
    googleEnv :: Gogol.Env GoogleScopes,
    config :: Config
  }