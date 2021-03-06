{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom

data Album = Album
  { name :: String,
    artist :: String,
    source :: String,
    tags :: [String]
  }

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

bodyElement :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
bodyElement = do
  el "h1" $ text "Reflex"
  el "h2" $ text "Album form"
  el "div" $ do
    el "div" $ do
      el "label" $ text "Name"
      _ <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ (T.pack name)
      return ()
    el "div" $ do
      el "label" $ text "Artist"
      _ <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ (T.pack artist)
      return ()
    el "div" $ do
      el "label" $ text "Source"
      _ <- dropdown source (constDyn sources) def
      return ()
    el "div" $ do
      el "label" $ text "Tags"
      forM tags tagElement
      _ <- button "Add more"
      return ()
    _ <- button "Submit"
    return ()
  where
    Album {..} = initialAlbum

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

tagElement :: (DomBuilder t m, PostBuild t m) => String -> m ()
tagElement tag = do
  el "div" $ do
    _ <-
      inputElement $
        def
          & inputElementConfig_initialValue .~ (T.pack tag)
    _ <- button "Remove"
    return ()
