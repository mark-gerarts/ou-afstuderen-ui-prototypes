module Main where

import Prelude
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
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
    , source :: Source
    , tags :: Array Tag
    }

type Tag
  = String

data Source
  = CD
  | LP
  | Digital

derive instance eqSource :: Eq Source

instance showSource :: Show Source where
  show CD = "CD"
  show LP = "LP"
  show Digital = "Digital"

data Action
  = NoOp
  | AddTag

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
        { name: "The Dark Side of the Moon"
        , artist: "Pink Floyd"
        , source: Digital
        , tags: [ "Rock" ]
        }
    }

handleAction :: forall a. MonadState State a => Action -> a Unit
handleAction = case _ of
  NoOp -> H.modify_ \state -> state
  AddTag -> H.modify_ \state -> state { album = addTag state.album "" }

addTag :: Album -> Tag -> Album
addTag album tag = album { tags = album.tags <> [ tag ] }

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
    , HH.div_
        [ HH.label_ [ HH.text "Source" ]
        , HH.select_ (map (renderSourceOption album.source) [ CD, LP, Digital ])
        ]
    , HH.div_
        ( [ HH.label_ [ HH.text "Tags" ] ]
            <> (map renderTag album.tags)
            <> [ HH.button [ HE.onClick \_ -> Just AddTag ] [ HH.text "Add more" ] ]
        )
    ]

renderSourceOption :: forall a. Source -> Source -> HH.HTML a Action
renderSourceOption activeSource source =
  HH.option
    [ HP.value $ show source
    , HP.selected $ source == activeSource
    ]
    [ HH.text $ show source ]

renderTag :: forall a. Tag -> HH.HTML a Action
renderTag tag =
  HH.div_
    [ HH.input [ inlineBlock, HP.value tag ]
    , HH.button [ inlineBlock ] [ HH.text "Remove" ]
    ]

inlineBlock :: forall a b. HP.IProp a b
inlineBlock = HP.prop (HH.PropName "style") "display: inline-block"
