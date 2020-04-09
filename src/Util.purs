module Util where

import Prelude
import Effect
  
foreign import copyTextToClipboard :: String -> Effect Unit
foreign import eval :: String -> Effect Unit

