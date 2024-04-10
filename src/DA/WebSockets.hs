module DA.WebSockets where

import Control.Monad
import DA.REST
import DA.WebSockets.Types
import Data.Aeson (encode)
import Data.Aeson.Decoding (decode)
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Text (Text)
import Deriving.Aeson
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, readTMVar, writeTQueue)
import Effectful.Log
import Effectful.Reader.Static
import Env
import Network.OAuth.OAuth2
import Network.WebSockets (DataMessage (Binary, Text), WebSocketsData (..))
import Network.WebSockets.Connection
import PyF
import Servant.Client (runClientM)
import Servant.Client.Core
import UnliftIO.Retry
import Wuss
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

type WebSocketEffects es = (IOE :> es, Concurrent :> es, Reader Env :> es, Log :> es)

newtype InitialParams = InitialParams
  { token :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] InitialParams

data ToWebSocketMsg a = ToWebSocketMsg
  { params :: a,
    id :: Int,
    method :: Maybe Int
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] (ToWebSocketMsg a)

instance (ToJSON a, FromJSON a) => WebSocketsData (ToWebSocketMsg a) where
  fromDataMessage a = case a of
    Text bytestring _ -> fromJust $ decode bytestring
    Binary bytestring -> fromJust $ decode bytestring
  fromLazyByteString bytestring = fromJust $ decode bytestring
  toLazyByteString = encode

data CentrifugoResponseResult = CentrifugoResponseResult
  { client :: Text,
    version :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] CentrifugoResponseResult

data PrivateInfo = PrivateInfo
  { user :: Text,
    client :: Text
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] PrivateInfo

data PrivateResult = PrivateResult
  { _type :: Int,
    channel :: Text,
    _data :: PrivateInfo
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] PrivateResult

data FromWebSocketMsg a = FromWebSocketMsg
  { id :: Int,
    result :: a
  }
  deriving (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] (FromWebSocketMsg a)

instance (ToJSON a, FromJSON a) => WebSocketsData (FromWebSocketMsg a) where
  fromDataMessage msg = case msg of
    Text bytestring _ -> fromJust $ decode bytestring
    Binary bytestring -> fromJust $ decode bytestring
  fromLazyByteString bytestring = fromJust $ decode bytestring
  toLazyByteString = encode

runWS :: (WebSocketEffects es, Reader UserProfile :> es) => Eff es ()
runWS = withEffToIO SeqUnlift $ \unlift ->
  runSecureClient "centrifugo.donationalerts.com" 443 "/connection/websocket" (unlift . wsClient)

--Repeated guide from https://hackage.haskell.org/package/wuss
wsClient :: (WebSocketEffects es, Reader UserProfile :> es) => Connection -> Eff es ()
wsClient connection = do
  userProfile <- ask @UserProfile
  let msgToWS = ToWebSocketMsg {params = InitialParams {token = userProfile.socketConnectionToken}, id = 1, method = Nothing}
  liftIO $ sendTextData connection msgToWS

  msg <- liftIO $ receiveData @(FromWebSocketMsg CentrifugoResponseResult) connection
  logTrace_ "Successfully receive centrifugo client Msg"

  env <- ask @Env
  clientEnv <- getDAClientEnv
  oauth2token <- atomically $ readTMVar env.clientAccessToken
  let clientID = msg.result.client
      subscribeReqBody = SubscribeReqBody {channels = pure [fmt|$alerts:donation_{userProfile.id}|], client = clientID}
      accessToken = oauth2token.accessToken
      subscribeClient = daClient.subscribeToChannels (mkAuthenticatedRequest accessToken authenticateDAOAuth) subscribeReqBody
  subscribedChannelsRes <- liftIO $ runClientM subscribeClient clientEnv

  case subscribedChannelsRes of
    Left err -> logAttention_ [fmt|{show err}|]
    Right subscribedChannels -> do
      logTrace_ [fmt|Successfully received subscribed channels list with length:{length(subscribedChannels.channels)}|]
      for_ subscribedChannels.channels $ \subscribedChannel -> do
        let msgToConnect = ToWebSocketMsg {params = subscribedChannel, id = 2, method = Just 1}
        liftIO $ sendTextData connection msgToConnect

  forever $ do
    message <- liftIO $ receiveData connection
    case decode @NewDonationAlertMsg message of
      Nothing -> logTrace [fmt|I got some garbage from DA|] $ object ["message" .= TE.decodeUtf8 (BS.toStrict message)]
      Just donationMsg -> atomically $ writeTQueue env.donationAlertsChan donationMsg

runDAListener :: (WebSocketEffects es) => Eff es ()
runDAListener = do
  env <- ask
  clientEnv <- getDAClientEnv
  oauth2token <- atomically $ readTMVar env.clientAccessToken
  let accessToken = oauth2token.accessToken
      authenticatedUserInfo = daClient.userInfo $ mkAuthenticatedRequest accessToken authenticateDAOAuth
  userProfileRes <- liftIO $ runClientM authenticatedUserInfo clientEnv
  case userProfileRes of
    Left err -> logAttention_ [fmt|{show err}|]
    Right userProfile -> do
      logTrace_ "Successfully got UserInfo"
      let runWSRecovery = recoverAll retryPolicyDefault (const runWS)
      runReader userProfile._data runWSRecovery
