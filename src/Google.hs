{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Google where

import Control.Applicative
import Control.Monad (forever)
import Data.Aeson qualified as A
import Data.Aeson.Schema
import Data.Foldable (for_)
import Data.List.NonEmpty
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Time
import Data.Time.Format.ISO8601
import Donation
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Log
import Effectful.Reader.Static
import Env qualified as MyEnv
import Gogol
import Gogol.Sheets
import Gogol.YouTube
import PyF
import Text.URI (QueryParam (..))
import Text.URI qualified as URI

type GoogleEffects es = (IOE :> es, Concurrent :> es, Reader MyEnv.Env :> es, Log :> es)

newtype YoutubeID = YoutubeID {getYoutubeID :: Text}

data YoutubeLink = ShortLink URI.URI | LongLink URI.URI

getUriFromDonateMsg :: Text -> [YoutubeLink]
getUriFromDonateMsg msg =
  mapMaybe
    ( \uri -> case uri.uriAuthority of
        Left _ -> Nothing
        Right authority
          | authority.authHost == fromJust (URI.mkHost "youtu.be") -> Just (ShortLink uri)
          | authority.authHost `elem` fmap (fromJust . URI.mkHost) ["www.youtube.com", "youtube.com", "m.youtube.com"] -> Just (LongLink uri)
          | otherwise -> Nothing
    )
    $ allUri msg

getYoutubeIDFromUri :: YoutubeLink -> Maybe Text
getYoutubeIDFromUri =
  \case
    LongLink uri ->
      let queryValue =
            mapMaybe
              ( \case
                  QueryFlag _ -> Nothing
                  QueryParam key value
                    | key == fromJust (URI.mkQueryKey "v") -> Just (URI.unRText value)
                    | otherwise -> Nothing
              )
              uri.uriQuery
       in listToMaybe queryValue
    ShortLink uri -> case uri.uriPath of
      Nothing -> Nothing
      Just (_, a :| _) -> Just (URI.unRText a)

getYoutubeVideoFromID :: MonadUnliftIO m => MyEnv.GoogleEnv -> Text -> m VideoListResponse
getYoutubeVideoFromID googleEnv youtubeID  = do
  runResourceT $
    Gogol.send
      googleEnv
      ( (newYouTubeVideosList ["snippet", "contentDetails"])
          { id = Just [youtubeID]
          } ::
          YouTubeVideosList
      )

mainProcess :: (GoogleEffects es) => Eff es ()
mainProcess = forever $ do
  env <- ask
  donation <- atomically $ readTQueue env.donationAlertsChan
  logTrace_ [fmt|I got message|]

  let username = [get| donation.result.data.data.username |]
      rubAmount = [get| donation.result.data.data.amount_in_user_currency |] :: Double
      msg = [get| donation.result.data.data.message |]
      uris = getUriFromDonateMsg msg
      ids = mapMaybe getYoutubeIDFromUri uris
  videoListResponses <- traverse (getYoutubeVideoFromID env.googleEnv) ids
  logTrace_ [fmt|I got video from Youtube|]

  for_ videoListResponses $ \response -> do
    let videos = asum response.items
    for_ videos $ \video -> do
      let videoData = do
            cd <- video.contentDetails
            res <- cd.duration
            duration <- iso8601ParseM @_ @CalendarDiffTime (unpack res)
            snippet <- video.snippet
            title <- snippet.title
            pure (duration, title)
      case videoData of
        Nothing -> logAttention [fmt|I got youtube garbage from Google|] $ object ["videoData" .= videoData]
        Just (duration, title) -> do
          let minuteDuration = round $ max (nominalDiffTimeToSeconds duration.ctTime / 60) 1
              queue = realToFrac $ floor (rubAmount / (realToFrac minuteDuration * 20))
              valueRange =
                newValueRange
                  { majorDimension = Just ValueRange_MajorDimension_Rows,
                    values =
                      Just
                        [ [ A.String title,
                            A.Number $ realToFrac rubAmount,
                            A.Number $ realToFrac minuteDuration,
                            A.Number queue,
                            A.String username
                          ]
                        ]
                  } ::
                  ValueRange
              request =
                ( newSheetsSpreadsheetsValuesAppend
                    valueRange
                    env.config.tableName
                    env.config.tableID
                )
                  { insertDataOption = Just SpreadsheetsValuesAppendInsertDataOption_INSERTROWS,
                    valueInputOption = Just SpreadsheetsValuesAppendValueInputOption_USERENTERED
                  } ::
                  SheetsSpreadsheetsValuesAppend
          _ <- runResourceT $ Gogol.send env.googleEnv request
          logTrace_ [fmt|I made google sheets bigger|]
          pure ()
