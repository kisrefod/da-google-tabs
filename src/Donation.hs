module Donation where

import Text.URI qualified as URI
import Replace.Megaparsec
import Text.Megaparsec
import Data.Maybe (maybeToList)
import Control.Monad
import Data.Either (rights)
import Data.Void
import Data.Text

allUriP :: Parsec Void Text [Either Text URI.URI]
allUriP = sepCap URI.parser 

allUri :: Text -> [URI.URI]
allUri = rights . join . maybeToList . parseMaybe allUriP 

