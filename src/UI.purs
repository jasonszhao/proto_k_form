module UI where

import Prelude
import Control.Monad.State.Class
import Data.Array
import Data.Either (Either(..), either)
import Data.Foldable (intercalate, all)
import Data.Maybe
import Data.String as String
import Data.String.Common
import Data.String.Pattern
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ElemName(..), PropName(..), ClassName(..), AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (ParseError)
import Effect
import Data.Newtype
import Effect.Class.Console
import Effect.Random
import Test.Assert
import Halogen.Query.EventSource as ES
import Web.Event.Event as E
import Web.HTML.Event.HashChangeEvent.EventTypes as HET
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Event.Internal.Types
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.Window (document) as Web
import Effect.Aff (Aff)


import Tokens
import Compute
import State
import Util

foreign import changeHash :: String -> Effect Unit
foreign import getHash :: Effect String 


------------------
showParseResult :: ParseResult -> String
showParseResult (Left x) = "Parse error: " <> show x

showParseResult (Right x) = intercalate "\n" $ map show x

interpret :: String -> String
interpret input =
  let
    parsed :: Either ParseError Commands
    parsed = parse input

    runString :: Commands -> String
    runString c = intercalate "\n" (map show $ run c)
  in
    either show runString parsed

showComputedResult :: Either ParseError Expression -> String
-- showComputedResult (Left x) = "-"
-- showComputedResult (Right x) = show x
showComputedResult x = "none right now"

-------------------- UI ------------------
renderEditor :: forall m. State -> H.ComponentHTML Action () m
renderEditor state =
  let
    input = case state.current of 
      Just c -> c.text 
      Nothing -> ""

    parsed = showParseResult $ parse input

    result = interpret input
  in
    HH.div [ HP.class_ $ ClassName "w-60-l pa4-l" ]

      [ HH.div 
        [ HP.class_ $ ClassName $ "tr db mr3"]
        [ HH.a 
          [ HE.onClick $ \_ -> Just $ ChangeHash ""
          , HP.class_ $ ClassName $ "pa2 dib pointer black no-underline"
          , HP.attr (AttrName "disabled") (show (state.current == Nothing))
          ] [ HH.text "Create New" ]
        ] 
      , HH.div
          [ HP.class_ $ ClassName "flex flex-row"
          , HP.prop (PropName "style") "border: 1px solid green;"
          ]
          -- though rows will be min(max(10, lines in buffer + 2), some number)  
          [ HH.element (ElemName "textarea")
              [ HE.onValueInput $ Just <<< UpdateCurrentBuffer
              , HP.class_ $ ClassName "code pa2 cf bg-white flex-grow-1"
              , HP.prop (PropName "style") "font-size: 100%; line-height: 1.5; border: none; resize: none;"
              , HP.rows 10
              , HP.value input
              , HP.placeholder $ if state.current == Nothing then "(new)" else ""
              ]
              [ HH.text input ]
          , HH.div [ HP.class_ $ ClassName "w-40 pa2 code bg-near-black purple-80", HP.prop (PropName "style") "line-height: 1.5; min-height: 100%" ]
              ( result
                  # split (Pattern "\n")
                  # map \v ->
                      ( HH.div
                          [ HP.prop (PropName "style") "text-align: right"
                          , HP.class_ $ ClassName "pointer"
                          , HE.onClick $ \_ -> Just $ CopyToClipboard v
                          ]
                          [ HH.text v ]
                      )
              )
          ]
      ]

renderFileList :: forall m. State -> H.ComponentHTML Action () m
renderFileList state =
  let
    isEditingFile' :: SavedFile -> State -> Boolean
    isEditingFile' ({ name: fileName1 }) ({ current: Just { name: ( fileName2) } }) = fileName1 == fileName2
    isEditingFile' _ _ = false

    isEditingFile f = isEditingFile' f state
  in
    HH.div [ HP.class_ $ ClassName "w-40-l pv4 mt4 mt0-l" ]
      [ HH.div [ HP.class_ $ ClassName "pa2 db tr mr3 tc-l b normal-l" ] [ HH.text "Saved History" ]
      , ( if length state.files > 0 then
            HH.div [ HP.class_ $ ClassName "ba mh4", HP.prop (PropName "style") "line-height: 1.5;" ]
              ( state.files
                  # map \f ->
                      HH.a
                        [ HP.class_ $ ClassName $ "pa2 bb pointer flex db black no-underline" <> (if isEditingFile f then " bg-light-gray" else "")
                        , HP.href $ "#" <> f.name
                        ]
                        [ HH.span [ HP.class_ $ ClassName "flex-grow-1" ] [ HH.text f.name ]
                        , HH.text (if isEditingFile f then "(editing)" else "")
                        ]
              )
          else
            HH.div [ HP.class_ $ ClassName "ba mh4", HP.prop (PropName "style") "height: 4em;" ] []
        )
      --   [ HH.div [ HP.class_ $ ClassName "pa2 bb pointer bg-light-gray flex" ]
      --       [ HH.span [ HP.class_ $ ClassName "flex-grow-1" ] [ HH.text "(Feb. 19, 2020) X-ray Cryptophrantic-Zoolography" ]
      --       , HH.text "(editing)"
      --       ]
      --   , HH.div [ HP.class_ $ ClassName "pa2 pointer" ] [ HH.text "Diffraction and Interference" ]
      --   ]
        , HH.div [ HP.class_ $ ClassName "tc pa2 db mr3 pointer" 
                 , HE.onClick $ \_ -> Just $ ClearAll ] [ HH.text "Clear Everything"]
      ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  -- Main interactive area 
  HH.div [ HP.class_ $ ClassName "-debug-grid flex-l flex-row-l" ]
    [ renderEditor state
    , renderFileList state
    ]



--------- UI Changes ----------------

data Action
  -- state transitions
  = LoadInitialState
  | UpdateCurrentBuffer String
  | HashChange 
  | ClearAll
  | PrintState
  | ChangeHash String
  -- 
  | CopyToClipboard String


-- create new entry. load that entry into buffer. switch url hash
createNewFile :: State -> State
createNewFile = _ {current = Nothing}


updateWhere :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateWhere g new a = map (\old -> if g old then new else old) a

-- need to use Aff instead of MonadEffect m because `eventListenerEventSource` requires 
-- the typeclass MonadAff that only Aff implements
handleAction ∷ forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction action =
  let
    updateSavedFileFromCurrentBuffer :: State -> State
    updateSavedFileFromCurrentBuffer state = case state.current of
      Just ({ name: fileName, text }) ->
        let
          updatedSavedFile = { name: fileName, text: text }
        in
          case findIndex (\f -> f.name == fileName) state.files of
            Just i -> state { files = updateWhere (\f -> f.name == fileName) updatedSavedFile state.files }
            Nothing -> state { files = cons updatedSavedFile state.files }
      
      
      -- this should never happen
      Nothing -> state 
    
    loadFileIntoBuffer :: String -> State -> State
    loadFileIntoBuffer "" state = state { current = Nothing} 
    loadFileIntoBuffer fileName state =
      case find (\f -> f.name == fileName) state.files of
        Just savedFile -> state { current = Just { name: fileName, text: savedFile.text}}
        Nothing -> state { current = Just {name: fileName, text: ""}} -- file doesn't exist. create one. hmm.


    result = case action of

          -- appears done
          LoadInitialState -> do
            liftEffect $ log "attempting to load initial state"


            -- load from storage
            persisted <- liftEffect readFromStore
            liftEffect $ log $ "load persisted state: " <> show persisted
            put case persisted of
              Just p -> blankState { files = unwrap p }
              Nothing -> initialState
            
            -- set the current file based on the hash
            h <- liftEffect getHash
            H.modify_ $ loadFileIntoBuffer h

            -- subscribe to hash change events
            -- no idea how this code works
            w :: Window.Window
              <- liftEffect window

            H.subscribe' \sid -> 
              ES.eventListenerEventSource
                  HET.hashchange 
                  (Window.toEventTarget w) 
                  (map (\_ -> HashChange) <<< HCE.fromEvent)
                
            H.modify_ updateSavedFileFromCurrentBuffer


          UpdateCurrentBuffer s -> do
            state <- get

            liftEffect $ log $ "Update current buffer: " <> s

            -- update the current buffer contents
            case state.current of 

              -- name the current buffer if it's nameless
              Nothing -> do
                r <- liftEffect $ randomInt 10000 99999
                let fileName = show r
                H.modify_ (_ {current = Just { name: fileName, text: s}})
                liftEffect $ log $ "assign name to current buffer: " <> fileName
                liftEffect $ changeHash $ fileName
              Just {name} -> do 
                H.modify_ (_ { current = Just ({ name, text: s}) })


            H.modify_ updateSavedFileFromCurrentBuffer

            -- persist changes
            state <- get
            liftEffect $ persist $ wrap state.files

          ChangeHash h -> do
            liftEffect $ changeHash h

          HashChange -> do
            hash <- liftEffect getHash
            s <- get
  
            liftEffect $ log $ "hash change: " <> show hash

            H.modify_ $ loadFileIntoBuffer hash

          -- ---
          CopyToClipboard s -> liftEffect $ copyTextToClipboard s
          ClearAll -> do 
            liftEffect $ eval $ "localStorage.clear('" <> key <> "')"
            liftEffect $ eval $ "window.location.reload(false)"
          _ -> pure unit
  in 
    do 

      result
      
      state <- get
      
      -- liftEffect $ assert' "current file name can't be defined and blank" $
      --     state.current.name /= Just ""

      -- liftEffect $ assert' "no saved file names can be blank" $
      --   all (\f -> f.name /= "") state.files

      liftEffect $ log $ "state at the end of action: " <> show state
      liftEffect $ log "----------------------------------"
      

      



component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState: const blankState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just LoadInitialState }
    }
 -- app :: forall q i o m. H.Component HH.HTML q i o m -- app = H.mkComponent --   { initialState: const Nothing --   , render --   , eval: H.mkEval $ H.defaultEval { handleAction = handleAction } --   } --   where  --     render