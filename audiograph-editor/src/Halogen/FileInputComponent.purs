module Halogen.FileInputComponent where

-- | A halogen component for handling a file input button
-- | which handles the input by means of purescript-js-fileio
-- | (and which supports both text and binary input)

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Data.MediaType (MediaType)
import Effect.Aff (Aff)
import JS.FileIO (Filespec, loadTextFile, loadBinaryFileAsText)
import Halogen (IProp)
import Halogen.HTML.CSS (style)
import CSS.Display (display, displayNone)

-- | A simple file input control that wraps JS.FileIO
-- | Whether or not it is enabled may be set externally via a query

type Context = {
    componentId :: String     -- the component id
  , isBinary    :: Boolean    -- does it handle binary as text or just simple text
  , prompt      :: String     -- the user prompt
  , accept      :: MediaType  -- the accepted media type(s)
  }

data Query a =
    LoadFile a
  | UpdateEnabled Boolean a

data Message = FileLoaded Filespec

type State =
  { mfsp :: Maybe Filespec
  , isEnabled :: Boolean
  }

component :: Context -> H.Component HH.HTML Query Unit Message Aff
component ctx =
  H.component
    { initialState: const initialState
    , render: render ctx
    , eval: eval ctx
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { mfsp: Nothing
    , isEnabled : true
    }

  render :: Context  -> State -> H.ComponentHTML Query
  render ctx state =
    HH.span
      [ HP.class_ $ ClassName "fileInput" ]
      [ -- the label is a hack to allow styling of file input which is
        -- otherwise impossible - see https://stackoverflow.com/questions/572768/styling-an-input-type-file-button
        HH.label
             [ HP.for ctx.componentId
             , HP.class_ $ ClassName "hoverable fileInputLabel"
             -- , HP.class_ $ ClassName "fileInputLabel"
             ]
             [ HH.text ctx.prompt ]
        -- we set the style to display none so that the label acts as a button
      , HH.input
          [ HE.onChange (HE.input_ LoadFile)
          , HP.type_ HP.InputFile
          , HP.id_  ctx.componentId
          , HP.accept ctx.accept
          , HP.enabled state.isEnabled
          , noDisplayStyle
          ]
      ]

  eval :: Context -> Query ~> H.ComponentDSL State Query Message Aff
  eval ctx = case _ of
    LoadFile next -> do
      filespec <-
         if ctx.isBinary then
           H.liftAff $ loadBinaryFileAsText ctx.componentId
         else
           H.liftAff $ loadTextFile ctx.componentId
      _ <- H.modify (\state -> state { mfsp = Just filespec } )
      H.raise $ FileLoaded filespec
      pure next
    UpdateEnabled isEnabled next -> do
      _ <- H.modify (\state -> state {isEnabled = isEnabled})
      pure next

  noDisplayStyle :: ∀ i r. IProp (style :: String | r) i
  noDisplayStyle =
    style do
      display displayNone
