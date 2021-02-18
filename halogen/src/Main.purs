module Main where

import Prelude
import Control.Monad.State (class MonadState)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { album :: Album }

type Album
  = { name :: String
    , artist :: String
    }

data Action
  = NoOp

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { album:
        { name: "The Dark Side of The Moon"
        , artist: "Pink Floyd"
        }
    }

handleAction :: forall a. MonadState State a => Action -> a Unit
handleAction = case _ of
  NoOp -> H.modify_ \state -> state

render :: forall a. State -> HH.HTML a Action
render state =
  HH.div_ -- `div_ [..]` is a shorthand for `div [] [..]`
    [ HH.h1_ [ HH.text "Purescript + Halogen" ]
    , HH.h2_ [ HH.text "Album form" ]
    , renderAlbumForm state
    ]

renderAlbumForm :: forall a. State -> HH.HTML a Action
renderAlbumForm { album } =
  HH.div
    []
    [ HH.div_
        [ HH.label_ [ HH.text "Name" ]
        , HH.input [ HP.value album.name ]
        ]
    , HH.div_
        [ HH.label_ [ HH.text "Artist" ]
        , HH.input [ HP.value album.artist ]
        ]
    ]
