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
import Miso
import Miso.String

type Model = Int

data Action = SubmitForm | NoOp deriving (Show, Eq)

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
    model = 0
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel SubmitForm m =
  m <# do consoleLog "The form is submitted" >> pure NoOp

viewModel :: Model -> View Action
viewModel x =
  form_
    [onSubmit SubmitForm]
    [ div_
        []
        [ label_ [for_ "album"] [text "Album"],
          input_ [id_ "album"]
        ],
      div_
        []
        [ label_ [for_ "artist"] [text "Artist"],
          input_ [id_ "artist"]
        ],
      div_
        []
        [ label_ [for_ "source"] [text "Source"],
          select_
            [id_ "source"]
            [ option_ [value_ "cd"] [text "CD"],
              option_ [value_ "lp"] [text "LP"],
              option_ [value_ "digital"] [text "Digital"]
            ]
        ],
      div_ -- @todo: add more
        []
        [ label_ [for_ "tags"] [text "Tags"],
          input_ [id_ "tags"]
        ],
      button_
        []
        [text "Submit"]
    ]
