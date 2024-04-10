{-# LANGUAGE QuasiQuotes #-}

module DA.Auth where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.OAuth.OAuth2 (OAuth2 (..), ExchangeToken (..))
import Network.OAuth.OAuth2 qualified as OAuth2

import URI.ByteString qualified as URIBS
import URI.ByteString.QQ (uri) 
import DA.REST
import Servant.Server.Generic
import Effectful
import Effectful.Reader.Static
import Env
import Servant (NoContent (..))
import Effectful.Concurrent   
import Effectful.Concurrent.STM
import UnliftIO.STM (writeTMVar)
import Effectful.Log
import PyF

type OAuthEffects es = (IOE :> es, Concurrent :> es, Reader Env :> es, Reader OAuth2 :> es, Log :> es)

runDAOauth2 :: (Reader Env :> es) => Eff (Reader OAuth2 : es) b -> Eff es b
runDAOauth2 action = do
  env <- ask @Env
  runReader
    OAuth2
      { oauth2ClientId = env.config.oauth2ClientId,
        oauth2ClientSecret = env.config.oauth2ClientSecret,
        oauth2AuthorizeEndpoint = [uri|https://www.donationalerts.com/oauth/authorize|],
        oauth2TokenEndpoint = [uri|https://www.donationalerts.com/oauth/token|],
        oauth2RedirectUri = [uri|http://localhost:12354/oauth2/callback|]
      }
      action

authorizeUrl :: OAuthEffects es => Eff es (URIBS.URIRef URIBS.Absolute)
authorizeUrl = do 
  oauth <- ask @OAuth2 
  pure $ OAuth2.appendQueryParams
    [("scope", "oauth-user-show oauth-donation-subscribe")]
    $ OAuth2.authorizationUrl oauth


oauthRoutesHandle :: (OAuthEffects es) => OAuthRoutes (AsServerT (Eff es))
oauthRoutesHandle =
  OAuthRoutes
    { callback = oauthCallbackHandler
    }

oauthCallbackHandler :: OAuthEffects es => Text -> Eff es NoContent
oauthCallbackHandler code = do
  env <- ask @Env
  daAuth <- ask @OAuth2
  manager <- liftIO getGlobalManager
  let exchangeToken = ExchangeToken code
  accessToken <- runExceptT $ OAuth2.fetchAccessToken manager daAuth exchangeToken
  case accessToken of
    Left err -> logAttention_ [fmt|{show err}|]
    Right accessToken' -> atomically $ writeTMVar env.clientAccessToken accessToken'
  pure NoContent
