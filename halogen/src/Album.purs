module Album where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core as J
import Data.Array (drop, take, updateAt)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)

type Album
  = { name :: String
    , artist :: String
    , source :: Source
    , tags :: Array Tag
    }

data Source
  = CD
  | LP
  | Digital

type Tag
  = String

derive instance eqSource :: Eq Source

instance showSource :: Show Source where
  show CD = "CD"
  show LP = "LP"
  show Digital = "Digital"

instance encodeJsonSource :: EncodeJson Source where
  encodeJson = encodeJson <<< show

instance decodeJsonSource :: DecodeJson Source where
  decodeJson json = do
    string <- decodeJson json
    Right $ sourceFromString string

addTag :: Album -> Tag -> Album
addTag album tag = album { tags = album.tags <> [ tag ] }

updateTag :: Album -> Int -> Tag -> Album
updateTag album i tag = album { tags = updatedTags }
  where
  updatedTags = fromMaybe album.tags (updateAt i tag album.tags)

removeTag :: Album -> Int -> Album
removeTag album i = album { tags = take i album.tags <> drop (i + 1) album.tags }

sourceFromString :: String -> Source
sourceFromString x = case x of
  "CD" -> CD
  "LP" -> LP
  _ -> Digital

albumToJson :: Album -> J.Json
albumToJson = encodeJson

albumFromJson :: J.Json -> Either String Album
albumFromJson = decodeJson
