module UI where

import Prelude
import Control.Monad.State.Class
import Data.Array
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Maybe
import Data.String as String
import Data.String.Common
import Data.String.Pattern
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ElemName(..), PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Text.Parsing.StringParser (ParseError)
import Effect
import Data.Newtype
import Effect.Class.Console
import Effect.Random

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
    input = state.current.text # fromMaybe ""

    parsed = showParseResult $ parse input

    result = interpret input
  in
    HH.div [ HP.class_ $ ClassName "w-60-l pa4-l" ]
      [ HH.a [ HP.class_ $ ClassName "tr pa2 db mr3 pointer" ] [ HH.text "Create New" ]
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
    isEditingFile' ({ name: fileName1 }) ({ current: { name: (Just fileName2) } }) = fileName1 == fileName2

    isEditingFile' _ _ = false

    isEditingFile f = isEditingFile' f state
  in
    HH.div [ HP.class_ $ ClassName "w-40-l pv4 mt4 mt0-l" ]
      [ HH.div [ HP.class_ $ ClassName "pa2 db tr mr3 tc-l b normal-l" ] [ HH.text "Saved History" ]
      , ( if length state.files > 0 then
            HH.div [ HP.class_ $ ClassName "ba mh4", HP.prop (PropName "style") "line-height: 1.5;" ]
              ( state.files
                  # map \f ->
                      HH.div
                        [ HP.class_ $ ClassName $ "pa2 bb pointer flex" <> (if isEditingFile f then " bg-light-gray" else "")
                        , HE.onClick $ \_ -> Just $ LoadFromFiles f.name
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
  | LoadFromFiles String
  | CreateNewBuffer
  | HashChange String
  -- 
  | CopyToClipboard String


newFile :: String -> Effect File 
newFile filename = do 
  pure { name: Just filename, text: Nothing}

-- create new entry. load that entry into buffer. switch url hash
createNewFile :: State -> Effect State
createNewFile s = do 
  r <- randomInt 10000 99999
  let fileName = show r

  changeHash fileName
  f <- newFile fileName
  
  pure $ s {current = f}


updateWhere :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateWhere g new a = map (\old -> if g old then new else old) a

handleAction ∷ forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction =
  let
    updateFileFromCurrentBuffer :: State -> State
    updateFileFromCurrentBuffer state = case state.current of
      ({ name: Just fileName, text }) ->
        let
          updatedSavedFile = { name: fileName, text: fromMaybe "" text }
        in
          case findIndex (\f -> f.name == fileName) state.files of
            Just i -> state { files = updateWhere (\f -> f.name == fileName) updatedSavedFile state.files }
            Nothing -> state { files = cons updatedSavedFile state.files }
      ({ name: Nothing, text }) -> state { files = cons { name: "", text: fromMaybe "" text } state.files }

  in
    case _ of

      -- appears done
      LoadInitialState -> do
        r <- liftEffect random
        persisted <- liftEffect readFromStore

        log "loading initial state"

        put case persisted of
          Just p -> blankState { files = unwrap p }
          Nothing -> initialState
        
        h <- liftEffect getHash
        when (String.length h > 0) $ 
          H.modify_ (\s -> s {current { name =  Just h}})


        H.modify_ updateFileFromCurrentBuffer


      UpdateCurrentBuffer s -> do
        H.modify_ (\state -> state { current { text = Just s } })
        H.modify_ updateFileFromCurrentBuffer
        state <- get
        liftEffect $ persist $ wrap state.files

      -- appears done
      CreateNewBuffer -> do
        s <- get
        s' <- liftEffect $ createNewFile s
        put s'

      -- appears done
      HashChange hash -> do 
        s <- get
        unless (Just hash == s.current.name) do
          H.modify_ (\state -> state {current { name = if (String.length hash > 0) then Just hash else Nothing }})
          H.modify_ updateFileFromCurrentBuffer

      -- ---
      CopyToClipboard s -> liftEffect $ copyTextToClipboard s

      _ -> pure unit



component :: forall q i o m. (MonadEffect m) => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const blankState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just LoadInitialState }
    }
 -- app :: forall q i o m. H.Component HH.HTML q i o m -- app = H.mkComponent --   { initialState: const Nothing --   , render --   , eval: H.mkEval $ H.defaultEval { handleAction = handleAction } --   } --   where  --     render