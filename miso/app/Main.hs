{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import Control.Monad.IO.Class
import qualified Data.Map as M
import Miso
import Miso.String hiding (map)

data Model = Model {album :: Album} deriving (Eq)

data Album = Album
  { name :: String,
    artist :: String,
    source :: Source,
    tags :: [Tag]
  }
  deriving (Eq)

data Source = CD | LP | Digital deriving (Show, Eq)

type Tag = String

data Action = SubmitForm | AddTag | NoOp deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model = demoModel
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

initialModel :: Model
initialModel =
  Model
    { album =
        Album
          { name = "",
            artist = "",
            source = CD,
            tags = []
          }
    }

demoModel :: Model
demoModel =
  Model
    { album =
        Album
          { name = "AlbumName",
            artist = "ArtistName",
            source = Digital,
            tags = ["First Tag"]
          }
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel SubmitForm m =
  m <# do consoleLog "The form is submitted" >> pure NoOp
updateModel AddTag model@(Model {album = album@(Album {..})}) =
  noEff (model {album = album {tags = tags ++ [""]}})

viewModel :: Model -> View Action
viewModel (Model {album = Album {..}}) =
  div_ -- I'd rather use a form + onSubmit here, but JSaddle bugs with preventDefault...
    []
    [ link_
        [ rel_ "stylesheet",
          href_ "https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"
        ],
      h1_ [] [text "Miso"],
      h2_ [] [text "Album form"],
      div_
        []
        [ label_ [for_ "album"] [text "Name"],
          input_ [id_ "album", value_ $ toMisoString name]
        ],
      div_
        []
        [ label_ [for_ "artist"] [text "Artist"],
          input_ [id_ "artist", value_ $ toMisoString artist]
        ],
      div_
        []
        [ label_ [for_ "source"] [text "Source"],
          select_
            [id_ "source"]
            [ option_ [value_ "cd", selected_ (source == CD)] [text "CD"],
              option_ [value_ "lp", selected_ (source == LP)] [text "LP"],
              option_ [value_ "digital", selected_ (source == Digital)] [text "Digital"]
            ]
        ],
      div_
        []
        ([label_ [] [text "Tags"]] ++ map tagToInput tags ++ [button_ [onClick AddTag] [text "Add more"]]),
      button_
        [onClick SubmitForm]
        [text "Submit"]
    ]

inlineBlock :: Attribute action
inlineBlock = style_ $ M.singleton "display" "inline-block"

tagToInput :: Tag -> View action
tagToInput tag =
  div_
    []
    [ input_ [id_ "tags", inlineBlock, value_ $ toMisoString tag],
      button_ [inlineBlock] [text "Remove"] -- @todo: implement remove
    ]
