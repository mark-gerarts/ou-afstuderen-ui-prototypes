{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Map as M
import GHC.Generics
import JavaScript.Web.XMLHttpRequest
import Miso
import Miso.String (MisoString, fromMisoString, pack, toMisoString)

data Model = Model {album :: Album} deriving (Eq)

data Album = Album
  { name :: String,
    artist :: String,
    source :: Source,
    tags :: [Tag]
  }
  deriving (Eq, Generic, ToJSON)

data Source = CD | LP | Digital deriving (Show, Eq, Generic, ToJSON)

type Tag = String

data Action
  = SubmitForm
  | AddTag
  | RemoveTag Int
  | UpdateTag Int MisoString
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main = startApp App {..}
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
updateModel AddTag model@(Model {..}) =
  noEff (model {album = addTag album})
updateModel (RemoveTag i) model@(Model {..}) =
  noEff (model {album = removeTag album i})
updateModel (UpdateTag i tag) model@(Model {..}) =
  noEff (model {album = updateTag album i (fromMisoString tag)})
updateModel SubmitForm model@(Model {..}) =
  model <# do xhrByteString req >> pure NoOp
  where
    req =
      Request
        { reqMethod = POST,
          reqURI = pack "https://reqres.in/api/users", -- Just some dummy REST API
          reqLogin = Nothing,
          reqHeaders = [],
          reqWithCredentials = False,
          reqData = StringData $ pack $ Char8.unpack $ encode album
        }

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
        ( [label_ [] [text "Tags"]]
            ++ ( map
                   (uncurry viewTag)
                   (zip tags [0 ..])
               )
            ++ [button_ [onClick AddTag] [text "Add more"]]
        ),
      button_
        [onClick SubmitForm]
        [text "Submit"]
    ]

viewTag :: Tag -> Int -> View Action
viewTag tag i =
  div_
    []
    [ input_ [id_ "tags", inlineBlock, value_ $ toMisoString tag, onInput $ UpdateTag i],
      button_ [inlineBlock, onClick $ RemoveTag i] [text "Remove"]
    ]

inlineBlock :: Attribute Action
inlineBlock = style_ $ M.singleton "display" "inline-block"

addTag :: Album -> Album
addTag Album {..} = Album {tags = tags ++ [""], ..}

removeTag :: Album -> Int -> Album
removeTag (Album {..}) i = Album {tags = take i tags ++ drop (i + 1) tags, ..}

updateTag :: Album -> Int -> String -> Album
updateTag (Album {..}) i tag = Album {tags = take i tags ++ [tag] ++ drop (i + i) tags, ..}
