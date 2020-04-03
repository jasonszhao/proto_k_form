module State where

import Prelude
import Effect
import Data.Maybe
import Effect.Class.Console
import Data.Generic.Rep (class Generic)
import Foreign.Generic (defaultOptions, genericEncodeJSON, genericDecodeJSON, Options)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Data.Either
import Control.Monad.Except (runExcept)
import Data.String.Read
import Data.String
import Data.Newtype



----------- State definition and associated functions ----------------
type State
  = { current :: File
    , files :: Array SavedFile
    }

-- Invariant: if the current file is not empty, then it should be saved in the files list

type File
  = { text :: Maybe String
    , name :: Maybe String
    }

type SavedFile  -- is there a way to lift record fields out of Monads?
  = { text :: String
    , name :: String
    }

blankState :: State
blankState = {current: {text: Nothing, name: Nothing }, files: [] }
initialState :: State
initialState =
  { current:
    { text:
      Just
        """(= x [3 +/- 0.1])
(* 2 x)
(/ (* (cos 20) (tan 20)) (sin 20))"""
    , name: Just "(Feb. 19, 2020) X-ray Cryptophrantic-Zoolography"
    }
  , files:
    [ { name: "Diffraction and Interference", text: "" }
    ]
  }


isFileBlank :: File -> Boolean
isFileBlank { text: Just x } | length x > 0 = true
isFileBlank _                               = false



-- ========= State persistence ====================
newtype SavedState
  = SavedState (Array SavedFile)

derive instance genericSavedState :: Generic SavedState _
derive instance newTypeSavedState :: Newtype SavedState _

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

instance showSavedState :: Show SavedState where
  show =  genericEncodeJSON opts

instance readSavedState :: Read SavedState where
  read s = hush $ runExcept $ genericDecodeJSON opts s

key :: String 
key = "ps_kenyon_form-savedFiles"

persist :: SavedState -> Effect Unit
persist state = do
  w <- window
  s <- localStorage w
  setItem key (show state) s
  log $ "persisted: " <> (show state)
  pure unit

readFromStore :: Effect (Maybe SavedState)
readFromStore = do
  w <- window
  s <- localStorage w
  r <- getItem key s
  pure (read =<< r)

