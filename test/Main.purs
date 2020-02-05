module Test.Main where

import Prelude (Unit, discard, pure, show, unit, ($), (/), (<>))
import Tokens (Command(..), Declaration(..), Expression(..), Operator(..), command, commands, declaration, expr, number, quantity)

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.StringParser (runParser)
import Data.List (List, fromFoldable)
import Effect.Exception (error)

import Control.Monad.Error.Class (throwError)

import Math as Math
import Compute

type Array = List

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "parsing" do
          describe "numbers" do
            it "" $ runParser number "3.0" `shouldEqual` Right 3.0
            it "takes haskell representation"
              $ quickCheck \n -> ((runParser number (show n)) === Right n)
          describe "quantities" do
            it "takes quantities"
              $ quickCheck \a ->
                  let
                    b = a / 1000.0
                  in
                    ( (runParser quantity ("[ " <> (show a) <> " +/- " <> (show b) <> " ]"))
                        === Right (Quantity a b)
                    )
          describe "expr" do
            describe "takes valid addition" do
              it "1" $ case runParser expr "(+ hello 2)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

              it "2" $ case runParser expr "(+ 3 2)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

              it "2" $ case runParser expr "(+ 3 world 5)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x
          

          describe "declaration" do
            describe "takes valid declaration" do
              it "1" $ case runParser declaration "(= hello 2)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

          -- describe "compute" do
          --   let compute_ = compute constants
          --   it "1" $ quickCheck \a -> (compute (Exec Sub (fromFoldable [
          --       (Exec Sin $ fromFoldable [(Quantity a 0.0)]), Quantity (Math.sin a) 0.0])) === 0)
          describe "command" do
            describe "takes valid commands" do
              it "1" $ case runParser command "(= hello 2)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

              it "2" $ case runParser command "(+ 3 2)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

              it "2" $ case runParser command "(+ 3 world 5)" of
                  Right _ ->  pure unit
                  Left x -> throwError $ error $ show x

          describe "entirety" do
            it "takes valid inputs"
              $ runParser commands "(= hello 1) (+ hello 2)"
                  `shouldEqual`
                    Right (fromFoldable
                      [ D $ Declaration "hello" (Quantity 1.0 0.0)
                      , E $ Exec Add $ fromFoldable [ Var "hello", Quantity 2.0 0.0 ]
                      ])
