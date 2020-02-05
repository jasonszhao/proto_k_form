module Main where

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
import Prelude (class Eq, class Ord, class Show, Unit, bind, discard, identity, map, negate, not, pure, show, unit, void, ($), (*), (*>), (+), (<*), (<<<), (<>), (==), (||))
import Text.Parsing.StringParser (ParseError(..), Parser, runParser, fail)
import Text.Parsing.StringParser.CodePoints (regex, satisfy, string)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (fix, many, many1, option)
import Web.DOM.ParentNode (QuerySelector(..))

-- last got what I needed from http://jakewheat.github.io/intro_to_parsing/
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
type State
  = { enabled :: Boolean, text :: String }

data Action
  = Toggle
  | Update String

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { enabled: false
  , text:
    """(= x [3 +/- 0.1])
(* 2 x)
(/ (* (cos 20) (tan 20)) (sin 20))"""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    input = state.text :: String

    parsed = showParseResult $ parse state.text

    result = interpret state.text
  in
    HH.div [ HP.ref $ H.RefLabel "subapp" ]
      [ HH.pre []
          [ HH.text "Attempts to propagate error. \n\n"
          , HH.a [ HP.href "https://en.wikipedia.org/wiki/S-expression" ] [ HH.text "S-Expressions" ]
          , HH.text ". Supports +, -, *, /, ^, sin, cos, and tan as operators. Values can be expressed as 314e-2 or [3.5 +- .3]. \n\n"
          ]
      , HH.br_
      , HH.element (ElemName "textarea")
          [ HE.onValueInput $ Just <<< Update
          , HP.rows 10
          , HP.cols 50
          , HP.prop (PropName "style") "font-family: monospace; font-size: 100%;"
          ]
          [ HH.text input ]
      , HH.br_
      , HH.pre [ HP.prop (PropName "style") "font-weight: bold" ] [ HH.text "Parsed Input: " ]
      , HH.pre [] [ HH.text $ parsed ]
      , HH.br_
      , HH.pre [ HP.prop (PropName "style") "font-weight: bold" ] [ HH.text "Result: " ]
      , HH.pre [] [ HH.text result ]
      
      ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st { enabled = not st.enabled }
  Update s -> H.modify_ \state -> state { text = s }

main :: Effect Unit
main =
  HA.runHalogenAff do
    awaitLoad
    root <- HA.selectElement (QuerySelector "#app")
    io <- runUI component unit (unsafePartial $ fromJust root)
    pure unit
