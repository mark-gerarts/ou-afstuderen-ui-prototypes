{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics
import Reflex.Dom
import Reflex.Dom.Xhr

data Album = Album
  { name :: String,
    artist :: String,
    source :: String,
    tags :: [String]
  }
  deriving (Generic, ToJSON, FromJSON)

data Resp = RespOk | RespError deriving (Eq)

-- TODO's
-- - Collecting and submitting the entered data
-- - "Add more" component for tag entry
-- - Toggle "fetch data"-button's text and disabled status

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: (DomBuilder t m, PostBuild t m) => m ()
headElement = do
  el "title" $ text "Prototype Reflex"
  styleSheet "https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"
  where
    styleSheet link =
      elAttr
        "link"
        ( Map.fromList
            [ ("rel", "stylesheet"),
              ("type", "text/css"),
              ("href", link)
            ]
        )
        $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h1" $ text "Reflex"

  -- Album edit form.
  el "h2" $ text "Album form"
  el "div" $ do
    el "div" $ do
      el "label" $ text "Name"
      _ <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ (T.pack (name initialAlbum))
      return ()
    el "div" $ do
      el "label" $ text "Artist"
      _ <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ (T.pack (artist initialAlbum))
      return ()
    el "div" $ do
      el "label" $ text "Source"
      _ <- dropdown (source initialAlbum) (constDyn sources) def
      return ()
    el "div" $ do
      el "label" $ text "Tags"
      forM (tags initialAlbum) tagElement
      _ <- button "Add more"
      return ()

    -- Submitting the album.
    submitEvent <- button "Submit"
    let albumPostRequest = postJson "http://localhost:8080" initialAlbum
    _ <- performRequestAsync (tag (constant albumPostRequest) submitEvent)
    return ()

  -- Fetching an album from the server and displaying it.
  el "h2" $ text "Displaying JSON data"
  fetchDataEvent <- button "Fetch JSON data from server"
  let fetchDataRequest = xhrRequest "GET" "http://localhost:8080/album/1" def
  evResp <- performRequestAsync (tag (constant fetchDataRequest) fetchDataEvent)
  dynAlbum <- holdDyn Nothing $ fmap decodeXhrResponse evResp
  dyn $ displayAlbum <$> dynAlbum
  return ()

sources :: Map String T.Text
sources =
  Map.fromList
    [ ("cd", "CD"),
      ("lp", "LP"),
      ("digital", "Digital")
    ]

initialAlbum :: Album
initialAlbum =
  Album
    { name = "The Dark Side of the Moon",
      artist = "Pink Floyd",
      source = "digital",
      tags = ["Rock"]
    }

displayAlbum :: (MonadWidget t m) => Maybe Album -> m ()
displayAlbum Nothing = blank
displayAlbum (Just (Album {..})) = el "dl" $ do
  el "dd" $ text "Name"
  el "dt" $ text $ T.pack $ name
  el "dd" $ text "Artist"
  el "dt" $ text $ T.pack $ artist
  el "dd" $ text "Source"
  el "dt" $ text $ T.pack $ source
  el "dd" $ text "Tags"
  el "dt" $ text $ T.intercalate ", " $ map T.pack tags

tagElement :: (DomBuilder t m, PostBuild t m) => String -> m ()
tagElement tag = do
  el "div" $ do
    _ <-
      inputElement $
        def
          & inputElementConfig_initialValue .~ (T.pack tag)
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            .~ ("style" =: "display:inline-block")
    (_, _) <- elAttr' "button" ("style" =: "display:inline-block") $ do
      text "Remove"
    return ()

insertNew :: a -> [a] -> [a]
insertNew x xs = xs ++ [x]
