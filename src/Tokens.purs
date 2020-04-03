module Tokens where

-- 

import Control.Alt ((<|>))
import Data.Char.Unicode (isLetter, isDigit, isSpace)
import Data.Either (Either)
import Data.List (List, fromFoldable, intercalate, (:))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.String.Yarn (fromChars)
import Prelude ((#), (*), (/), otherwise, class Eq, class Ord, class Show, Unit, bind, discard, map, pure, show, unit, void, ($), (*>), (<$>), (<*), (<>), (==), (||))
import Text.Parsing.StringParser (ParseError, Parser, runParser, fail, try)
import Text.Parsing.StringParser.CodePoints (regex, satisfy, string)
import Text.Parsing.StringParser.CodeUnits (char)
import Text.Parsing.StringParser.Combinators (fix, many, many1, option, choice)
import Data.String (toLower)

import Math


-- last got what I needed from http://jakewheat.github.io/intro_to_parsing/

data Operator
  = Add
  | Mul
  | Exp
  | Mod
  --- "macro" (transformed) operators:
  | Sub
  | Div
  | Sin
  | Cos
  | Tan
  | Log10

derive instance eqOperator :: Eq Operator

derive instance ordOperator :: Ord Operator


data Expression
  = Exec Operator (List Expression)
  | Var String
  | Quantity Number Number

derive instance eqExpression :: Eq Expression

data Declaration
  = Declaration String Expression

derive instance eqDeclaration :: Eq Declaration

data Command
  = D Declaration
  | E Expression

derive instance eqCommand :: Eq Command

type Commands
  = List Command

type ParseResult
  = Either ParseError Commands

---- Parsers
var :: Parser String
var = do
  fc <- firstChar
  rest <- many nonFirstChar
  whitespace
  pure $ fromChars (fc : rest)
  where
  firstChar = satisfy (\a -> isLetter a || a == '_')

  nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

number :: Parser Number
number =
  let
    transform :: Maybe Number -> Parser Number
    transform (Just x) = pure x

    transform _ = fail "failed to parse as number: "
  in
    do
      n <- regex """([+-]?(?=\.\d|\d)(\d+)?(\.?\d*))([eE]([+-]?\d+))?"""
      let
        n' = (fromString n) :: Maybe Number
      let
        result = transform n'
      whitespace
      result

quantity :: Parser Expression
quantity =
  let
    plusMinus = do
      _ <- string "+-" <|> string "+/-" <|> string "±"
      whitespace
      pure unit

    sigmaPortion = do
      plusMinus
      number

    bracketedQuantity = do
      _ <- char '['
      whitespace
      val <- number
      err <- option 0.0 sigmaPortion
      _ <- char ']'
      whitespace
      pure $ Quantity val err

    unbracketedQuantity = do
      n <- number
      pure $ Quantity n 0.0
  in
    do
      bracketedQuantity <|> unbracketedQuantity

operator :: Parser Operator
operator =
  choice
    [ try ((char '+') *> (pure Add))
    , try ((string "^" <|> string "**") *> (pure Exp))
    , try ((char '*') *> (pure Mul))
    , try ((char '-') *> (pure Sub))
    , try ((char '/') *> (pure Div))
    , try ((string "sin") *> (pure Sin))
    , try ((string "cos") *> (pure Cos))
    , try ((string "tan") *> (pure Tan))
    , try ((string "log" <|> string "log10") *> (pure Log10))
    ]

whitespace :: Parser Unit
whitespace = do
  void $ many $ satisfy isSpace

execution :: Parser Expression
execution = do
  _ <- char '('
  whitespace
  head <- operator
  -- todo: take arity from head
  whitespace
  tail <- many1 (expr <* whitespace)
  _ <- char ')'
  pure $ Exec head $ fromFoldable tail

expr :: Parser Expression
expr =
  fix
    $ \p ->
        ( ( (quantity)
              <|> (Var <$> var)
              <|> execution
          )
            <* whitespace
        )

declaration :: Parser Declaration
declaration = do
  _ <- char '('
  whitespace
  _ <- char '='
  whitespace
  v <- var
  exp <- expr
  _ <- char ')'
  whitespace
  pure $ Declaration v exp

command :: Parser Command
command =
  let
    d = do
      x <- declaration
      pure $ D x

    e = do
      x <- expr
      pure $ E x
  in
    choice
      [ try d
      , try e
      ]
      <* whitespace

commands :: Parser Commands
commands = whitespace *> many command

parse :: String -> Either ParseError Commands
parse c = (runParser commands) $ toLower c

----------- printers ---------
shownArgs :: List Expression -> List String
shownArgs args = map show args

showArgs :: List Expression -> String
showArgs args = intercalate " " (shownArgs args)

toOneSigFig :: Number -> String
toOneSigFig x = 
  let tail = pow 10.0 $ floor $ log x / ln10
  in (floor $ x / tail) # (*) tail # show

roundedToUncertainty :: Number -> Number -> String
roundedToUncertainty v e = 
  let tail = pow 10.0 $ floor $ log e / ln10
  in (floor $ v / tail) # (*) tail # show 


kenyonForm :: Number -> Number -> String 
kenyonForm v e = 
  let relE = 100.0 * e / v
      in "[" <> roundedToUncertainty v e <> " ± " <> toOneSigFig e <> "] (" <> toOneSigFig relE <> "%)"


instance showExpression :: Show Expression where
  show (Var x) = show x
  show (Exec o args) = "(" <> (show o) <> " " <> (showArgs args) <> ")"
  show (Quantity v e)
    | e == 0.0 = show v
    | otherwise = kenyonForm v e 
      

instance showOperator :: Show Operator where
  show Add = "+"
  show Mul = "*"
  show Exp = "^"
  show Mod = "%"
  show Sub = "-"
  show Div = "/"
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show Log10 = "log10"

instance showDeclaration :: Show Declaration where
  show (Declaration v e) = "(= " <> show v <> " " <> show e <> ")"

instance showCommand :: Show Command where
  show (D d) = show d
  show (E e) = show e
