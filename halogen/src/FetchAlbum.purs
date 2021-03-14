module FetchAlbum (fetchAlbum) where

import Album (Album, albumFromJson)
import Prelude (Unit, bind, show, ($))
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = Maybe Album

data Action
  = FetchAlbum

fetchAlbum :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
fetchAlbum =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction FetchAlbum = do
  let
    onError = H.modify_ \state -> state
  r <- H.liftAff $ AX.get AXRF.json "http://localhost:8080/album/1"
  case r of
    Left err -> onError
    Right response -> case albumFromJson response.body of
      Left err -> onError
      Right album -> H.modify_ \_ -> Just album

render :: forall a. State -> HH.HTML a Action
render state =
  HH.div_
    [ HH.h2_ [ HH.text "Displaying JSON data" ]
    , maybe
        (HH.button [ HE.onClick \_ -> Just FetchAlbum ] [ HH.text "Fetch album from server" ])
        renderAlbum
        state
    ]

renderAlbum :: forall a. Album -> HH.HTML a Action
renderAlbum album =
  HH.dl_
    [ HH.dt_ [ HH.text "Title:" ]
    , HH.dd_ [ HH.text album.name ]
    , HH.dt_ [ HH.text "Artist:" ]
    , HH.dd_ [ HH.text album.artist ]
    , HH.dt_ [ HH.text "Source:" ]
    , HH.dd_ [ HH.text $ show album.source ]
    , HH.dt_ [ HH.text "Tags:" ]
    , HH.dd_ [ HH.text $ intercalate ", " album.tags ]
    ]
