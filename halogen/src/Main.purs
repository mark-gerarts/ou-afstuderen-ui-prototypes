module Main where

import Prelude
import FetchAlbum (fetchAlbum)
import SubmitAlbum (submitAlbum)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

type Slots
  = ( submitAlbum :: forall query. H.Slot query Void Int
    , fetchAlbum :: forall query. H.Slot query Void Int
    )

_submitAlbum = SProxy :: SProxy "submitAlbum"

_fetchAlbum = SProxy :: SProxy "fetchAlbum"

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI prototype unit body

prototype :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
prototype =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

-- Our prototype essentially contains 2 separate things:
--
-- - Editing an album and submitting it
-- - Fetching an album from the server and displaying it
--
-- We reflect this in our code by separating them in 2 components.
render :: forall state action m. MonadAff m => state -> H.ComponentHTML action Slots m
render _ =
  HH.div_
    [ HH.h1_ [ HH.text "Purescript + Halogen" ]
    , HH.slot _submitAlbum 0 submitAlbum {} absurd
    , HH.slot _fetchAlbum 0 fetchAlbum {} absurd
    ]
