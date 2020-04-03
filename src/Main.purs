module Main where

import Prelude -- (class Eq, class Ord, class Show, Unit, bind, discard, identity, map, negate, not, pure, show, unit, void, ($), (*), (*>), (+), (<*), (<<<), (<>), (==), (||))
import Tokens
import Compute
import Control.Alt ((<|>))
import Data.Bifunctor (bimap)
import Data.Char.Unicode (isLetter, isDigit, isSpace)
import Data.Either (Either(..), either)
import Data.Foldable (all, intercalate)
import Data.List (List, find, foldl, fromFoldable, (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (fromString)
import Data.String.Yarn (fromChars)
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ElemName(..), PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError(..), Parser, runParser, fail)
import Text.Parsing.StringParser.CodePoints (regex, satisfy, string)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (fix, many, many1, option)
import Web.DOM.ParentNode (QuerySelector(..))

import UI


main :: Effect Unit
main =
  HA.runHalogenAff do
    awaitLoad
    root <- HA.selectElement (QuerySelector "#app")
    io <- runUI component unit (unsafePartial $ fromJust root)
    pure unit



