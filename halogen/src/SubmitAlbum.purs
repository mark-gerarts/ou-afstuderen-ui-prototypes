module SubmitAlbum (submitAlbum) where

import AlbumForm
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Album (albumToJson)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, absurd, bind, pure, unit, ($))

type Slots
  = ( albumForm :: H.Slot Query Void Int )

type State
  = { submitted :: Boolean }

data Action
  = SubmitForm

_albumForm = SProxy :: SProxy "albumForm"

submitAlbum :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
submitAlbum =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { submitted: false
    }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction SubmitForm = do
  maybeAlbum <- H.query _albumForm 0 $ H.request GetAlbum
  case maybeAlbum of
    Just album -> do
      r <- H.liftAff $ AX.post_ "http://localhost:8080" (Just (RequestBody.json (albumToJson album)))
      H.modify_ \state -> state { submitted = true }
    Nothing -> pure unit

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { submitted } =
  HH.div_
    [ HH.h2_ [ HH.text "Album form" ]
    , HH.slot _albumForm 0 albumForm unit absurd
    , HH.button
        [ HE.onClick \_ -> Just SubmitForm, HP.disabled submitted ]
        [ HH.text $ if submitted then "Submitted!" else "Submit" ]
    ]
