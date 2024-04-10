{-# LANGUAGE QuasiQuotes #-}

module DA.REST where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import Deriving.Aeson
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import PyF
import Servant.API
import Servant.Client
import Servant.Client.Core (AuthClientData, Request)
import Servant.Client.Core qualified as Client
import Servant.Client.Generic

newtype OAuthRoutes route = OAuthRoutes
  { callback ::
      route
        :- "oauth2"
          :> "callback"
          :> QueryParam' '[Required, Strict] "code" Text
          :> GetNoContent
  }
  deriving (Generic)

newtype DAResponse a = DAResponse
  { _data :: a
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] (DAResponse a)

data UserProfile = UserProfile
  { id :: Int,
    code :: Text,
    name :: Text,
    avatar :: Text,
    email :: Text,
    socketConnectionToken :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] UserProfile

data SubscribeReqBody = SubscribeReqBody
  { channels :: [Text],
    client :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] SubscribeReqBody

data ChannelData = ChannelData
  { channel :: Text,
    token :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] ChannelData

newtype SubscribedChannels = SubscribedChannels
  { channels :: [ChannelData]
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] SubscribedChannels

data DARoutes route = DARoutes
  { userInfo ::
      route
        :- AuthProtect "da-oauth-token"
          :> "user"
          :> "oauth"
          :> Get '[JSON] (DAResponse UserProfile),
    subscribeToChannels ::
      route
        :- AuthProtect "da-oauth-token"
          :> "centrifuge"
          :> "subscribe"
          :> ReqBody '[JSON] SubscribeReqBody
          :> Post '[JSON] SubscribedChannels
  }
  deriving (Generic)

type instance AuthClientData (AuthProtect "da-oauth-token") = AccessToken

authenticateDAOAuth :: AccessToken -> Request -> Request
authenticateDAOAuth token req = Client.addHeader "Authorization" ([fmt|Bearer {token.atoken}|] :: Text) req

getDAClientEnv :: (MonadIO m) => m ClientEnv
getDAClientEnv = do
  manager <- liftIO getGlobalManager
  pure $ mkClientEnv manager (fromJust $ parseBaseUrl "https://www.donationalerts.com/api/v1")

daClient :: DARoutes (AsClientT ClientM)
daClient = genericClient @DARoutes