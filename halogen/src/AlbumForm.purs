module AlbumForm (albumForm, Query(..)) where

import Album (Album, Source(..), Tag, addTag, removeTag, sourceFromString, updateTag)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, bind, map, pure, show, ($), (<<<), (<>), (==))

data Query a
  = GetAlbum (Album -> a)

-- By extracting components we simplify each component's state.
type State
  = Album

-- We could have created a generic add-more component as well, for tags. But you
-- get the idea.
data Action
  = AddTag
  | RemoveTag Int
  | UpdateTag Int Tag
  | UpdateName String
  | UpdateArtist String
  | UpdateSource Source

albumForm :: forall input output m. MonadAff m => H.Component HH.HTML Query input output m
albumForm =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  initialState _ =
    { name: "The Dark Side of the Moon"
    , artist: "Pink Floyd"
    , source: Digital
    , tags: [ "Rock" ]
    }

handleQuery :: forall action a o m. Query a -> H.HalogenM State action () o m (Maybe a)
handleQuery (GetAlbum reply) = do
  album <- H.get
  pure $ Just (reply album)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  AddTag -> H.modify_ \album -> addTag album ""
  UpdateTag i tag -> H.modify_ \album -> updateTag album i tag
  RemoveTag i -> H.modify_ \album -> removeTag album i
  UpdateName name -> H.modify_ \album -> album { name = name }
  UpdateArtist artist -> H.modify_ \album -> album { artist = artist }
  UpdateSource source -> H.modify_ \album -> album { source = source }

render :: forall a. State -> HH.HTML a Action
render state =
  HH.div_
    [ renderAlbumForm state ]

renderAlbumForm :: forall a. State -> HH.HTML a Action
renderAlbumForm album =
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
        , HE.onClick \_ -> Just $ RemoveTag i
        ]
        [ HH.text "Remove" ]
    ]

inlineBlock :: forall a b. HP.IProp a b
inlineBlock = HP.prop (HH.PropName "style") "display: inline-block"
