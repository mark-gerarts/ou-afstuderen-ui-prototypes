module Main where

import Prelude
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut.Core as J
import Data.Array (drop, mapWithIndex, take, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
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
  = { album :: Album
    , submitted :: Boolean
    }

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

instance encodeJsonSource :: EncodeJson Source where
  encodeJson = encodeJson <<< show

data Action
  = NoOp
  | AddTag
  | RemoveTag Int
  | UpdateTag Int Tag
  | UpdateName String
  | UpdateArtist String
  | UpdateSource Source
  | SubmitForm

component :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
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
    , submitted: false
    }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  NoOp -> H.modify_ \state -> state
  AddTag -> H.modify_ \state -> state { album = addTag state.album "" }
  UpdateTag i tag -> H.modify_ \state -> state { album = updateTag state.album i tag }
  RemoveTag i -> H.modify_ \state -> state { album = removeTag state.album i }
  UpdateName name -> H.modify_ \state -> state { album = state.album { name = name } }
  UpdateArtist artist -> H.modify_ \state -> state { album = state.album { artist = artist } }
  UpdateSource source -> H.modify_ \state -> state { album = state.album { source = source } }
  SubmitForm -> do
    album <- H.gets _.album
    r <- H.liftAff $ AX.post_ "http://localhost:8080" (Just (RequestBody.json (albumToJson album)))
    H.modify_ \state -> state { submitted = true }

addTag :: Album -> Tag -> Album
addTag album tag = album { tags = album.tags <> [ tag ] }

updateTag :: Album -> Int -> Tag -> Album
updateTag album i tag = album { tags = updatedTags }
  where
  updatedTags = fromMaybe album.tags (updateAt i tag album.tags)

removeTag :: Album -> Int -> Album
removeTag album i = album { tags = take i album.tags <> drop (i + 1) album.tags }

albumToJson :: Album -> J.Json
albumToJson = encodeJson

render :: forall a. State -> HH.HTML a Action
render state =
  HH.div_ -- `div_ [..]` is a shorthand for `div [] [..]`
    [ HH.h1_ [ HH.text "Purescript + Halogen" ]
    , HH.h2_ [ HH.text "Album form" ]
    , renderAlbumForm state
    ]

renderAlbumForm :: forall a. State -> HH.HTML a Action
renderAlbumForm { album, submitted } =
  HH.div
    []
    [ HH.div_
        [ HH.label_ [ HH.text "Name" ]
        , HH.input
            [ HP.value album.name
            , HE.onValueInput $ Just <<< UpdateName
            ]
        ]
    , HH.div_
        [ HH.label_ [ HH.text "Artist" ]
        , HH.input
            [ HP.value album.artist
            , HE.onValueInput $ \x -> Just (UpdateArtist x)
            ]
        ]
    , HH.div_
        [ HH.label_ [ HH.text "Source" ]
        , HH.select
            [ HE.onValueChange $ Just <<< UpdateSource <<< sourceFromString ]
            (map (renderSourceOption album.source) [ CD, LP, Digital ])
        ]
    , HH.div_
        ( [ HH.label_ [ HH.text "Tags" ] ]
            <> (mapWithIndex renderTag album.tags)
            <> [ HH.button [ HE.onClick \_ -> Just AddTag ] [ HH.text "Add more" ] ]
        )
    , HH.button
        [ HE.onClick \_ -> Just SubmitForm, HP.disabled submitted ]
        [ HH.text $ if submitted then "Submitted!" else "Submit" ]
    ]

renderSourceOption :: forall a. Source -> Source -> HH.HTML a Action
renderSourceOption activeSource source =
  HH.option
    [ HP.value $ show source
    , HP.selected $ source == activeSource
    ]
    [ HH.text $ show source ]

renderTag :: forall a. Int -> Tag -> HH.HTML a Action
renderTag i tag =
  HH.div_
    [ HH.input
        [ inlineBlock
        , HP.value tag
        , HE.onValueInput $ Just <<< (UpdateTag i)
        ]
    , HH.button
        [ inlineBlock
        , HE.onClick \_ -> Just (RemoveTag i)
        ]
        [ HH.text "Remove" ]
    ]

inlineBlock :: forall a b. HP.IProp a b
inlineBlock = HP.prop (HH.PropName "style") "display: inline-block"

sourceFromString :: String -> Source
sourceFromString x = case x of
  "CD" -> CD
  "LP" -> LP
  _ -> Digital
