module Compute where

import Prelude (identity, map, negate, ($), (*), (+), (/), (==), (<$>), (-), otherwise)
import Data.Map (Map, empty, insert, lookup)
import Data.Map as Map
import Data.Foldable (all, foldl, find)
import Data.List (List(..), fromFoldable, reverse, (:), head, tail, (!!))
import Data.Maybe (Maybe(..), fromJust, maybe, fromMaybe)
import Tokens (Command(..), Commands, Declaration(..), Expression(..), Operator(..))
import Data.Tuple (Tuple(..), fst, snd)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Math as Math
import Tokens

l = fromFoldable

type Store
  = Map String Expression

constants :: Store
constants = Map.fromFoldable [ Tuple "pi" (Quantity Math.pi 0.0), Tuple "e" (Quantity Math.e 0.0) ]

data UnwrappedQuantity
  = UnwrappedQuantity Number Number

wrapQuantity :: UnwrappedQuantity -> Expression
wrapQuantity (UnwrappedQuantity v e) = Quantity v e

---------- Numeric utilities --------------
rel :: UnwrappedQuantity -> Number
rel (UnwrappedQuantity v e)
  | v == 0.0 = 0.0
  | otherwise = Math.abs (e / v)

abs :: Number -> Number -> Number
abs v e = Math.abs (e * v)

mathSum :: List Number -> Number
mathSum xs = foldl (+) 0.0 xs

mathProduct :: List Number -> Number
mathProduct xs = foldl (*) 1.0 xs

mathHypot :: List Number -> Number
mathHypot xs = Math.sqrt $ mathSum $ map (\x -> x * x) xs

mathFac :: Number -> Number
mathFac 0.0 = 1.0

mathFac x = x * (mathFac (x - 1.0))

---------- Exec Operators
addO :: List UnwrappedQuantity -> UnwrappedQuantity
addO xs =
  let
    vs = map (\(UnwrappedQuantity v _) -> v) xs

    es = map (\(UnwrappedQuantity _ e) -> e) xs
  in
    UnwrappedQuantity (mathSum vs) (mathHypot es)

-- todo: error computation is wrong
mulO :: List UnwrappedQuantity -> UnwrappedQuantity
mulO xs =
  let
    vs = map (\(UnwrappedQuantity v _) -> v) xs

    es = map rel xs

    v' = mathProduct vs
  in
    UnwrappedQuantity v' (v' * (mathHypot es))

expO :: List UnwrappedQuantity -> UnwrappedQuantity
expO args =
  -- assert (length args) === 2
  let
    (UnwrappedQuantity v e) = fromMaybe (UnwrappedQuantity 0.0 0.0) $ head args

    (UnwrappedQuantity exp _) = fromMaybe (UnwrappedQuantity 0.0 0.0) $ args !! 1

    val = Math.pow v exp

    relErr = exp * (rel (UnwrappedQuantity v e))
  in
    UnwrappedQuantity val (abs val relErr)

mod :: List UnwrappedQuantity -> UnwrappedQuantity
mod args =
  -- assert (length args) === 2
  let
    (UnwrappedQuantity v e) = fromMaybe (UnwrappedQuantity 0.0 0.0) $ head args

    (UnwrappedQuantity mod _) = fromMaybe (UnwrappedQuantity 0.0 0.0) $ args !! 1

    val = Math.remainder v mod

    relErr = Math.remainder e mod
  in
    UnwrappedQuantity val (abs val relErr)

--------- Transformers ------------
-- we will only do one pass
type Transformer
  = Expression -> Expression

transform :: Transformer
transform (Exec op args) = case op of
  Add -> Exec op (map transform args)
  Mul -> Exec op (map transform args)
  Mod -> Exec op (map transform args)
  Exp -> Exec op (map transform args)
  Sub ->
    let
      h_ = head args

      h = transform $ fromMaybe (Quantity 0.0 0.0) h_

      t = (map transform) <$> tail args
    in
      case t of
        Just t -> Exec Add (h : (map (\a -> (Exec Mul (l [ (Quantity (-1.0) 0.0), a ])))) t)
        Nothing -> (Exec Mul (l [ Quantity (-1.0) 0.0, h ]))
  Div ->
    let
      a = transform $ fromMaybe (Quantity 0.0 0.0) $ head args

      b = transform $ fromMaybe (Quantity 1.0 0.0) $ args !! 1
    in
      (Exec Mul (l [ a, (Exec Exp (l [ b, Quantity (-1.0) 0.0 ])) ]))
  Sin ->
    let
      h_ = transform $ fromMaybe (Quantity 0.0 0.0) $ head args
      h_shifted = Exec Add (l [Quantity Math.pi 0.0, h_])
      h_mod_2pi = Exec Mod (l [h_shifted, Quantity (2.0 * Math.pi) 0.0 ])
      h = Exec Add (l [Quantity (-Math.pi) 0.0, h_mod_2pi])
    in

      Exec Add
        ( l
            [ Exec Mul (l [ Quantity (1.0 / (mathFac 1.0)) 0.0, Exec Exp (l [ h, Quantity 1.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 3.0)) 0.0, Exec Exp (l [ h, Quantity 3.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (1.0 / (mathFac 5.0)) 0.0, Exec Exp (l [ h, Quantity 5.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 7.0)) 0.0, Exec Exp (l [ h, Quantity 7.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (1.0 / (mathFac 9.0)) 0.0, Exec Exp (l [ h, Quantity 9.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 11.0)) 0.0, Exec Exp (l [ h, Quantity 11.0 0.0 ]) ])
            ]
        )
  Cos ->
    let
      h_ = transform $ fromMaybe (Quantity 0.0 0.0) $ head args
      h_shifted = Exec Add (l [Quantity Math.pi 0.0, h_])
      h_mod_2pi = Exec Mod (l [h_shifted, Quantity (2.0 * Math.pi) 0.0 ])
      h = Exec Add (l [Quantity (-Math.pi) 0.0, h_mod_2pi])
    in

      Exec Add
        ( l
            [ Exec Mul (l [ Quantity (1.0 / (mathFac 0.0)) 0.0, Exec Exp (l [ h, Quantity 0.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 2.0)) 0.0, Exec Exp (l [ h, Quantity 2.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (1.0 / (mathFac 4.0)) 0.0, Exec Exp (l [ h, Quantity 4.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 6.0)) 0.0, Exec Exp (l [ h, Quantity 6.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (1.0 / (mathFac 8.0)) 0.0, Exec Exp (l [ h, Quantity 8.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (-1.0 / (mathFac 10.0)) 0.0, Exec Exp (l [ h, Quantity 10.0 0.0 ]) ])
            ]
        )
  Tan ->
    let
      h_ = transform $ fromMaybe (Quantity 0.0 0.0) $ head args

      h_shifted = Exec Add (l [Quantity (Math.pi/2.0) 0.0, h_])
      h_mod = Exec Mod (l [h_shifted, Quantity (Math.pi) 0.0 ])
      h = Exec Add (l [Quantity (-Math.pi/2.0) 0.0, h_mod])
    in

      Exec Add
        ( l
            [ Exec Mul (l [ Quantity (1.0 / 1.0) 0.0, Exec Exp (l [ h, Quantity 1.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (1.0 / 3.0) 0.0, Exec Exp (l [ h, Quantity 3.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (2.0 / 15.0) 0.0, Exec Exp (l [ h, Quantity 5.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (17.0 / 315.0) 0.0, Exec Exp (l [ h, Quantity 7.0 0.0 ]) ])
            , Exec Mul (l [ Quantity (62.0 / 2835.0) 0.0, Exec Exp (l [ h, Quantity 9.0 0.0 ]) ])
            ]
        )
   -- todo: log10
  x -> Exec op (map transform args)

transform x = x

q :: Number -> Expression
q a = Quantity a 0.0

------
operatorsList :: List ({ token :: Operator, function :: List UnwrappedQuantity -> UnwrappedQuantity, arity :: Int })
operatorsList =
  l
    [ ({ token: Add, function: addO, arity: -1 })
    , ({ token: Mul, function: mulO, arity: -1 })
    , ({ token: Exp, function: expO, arity: 2 })
    , ({ token: Mod, function: mod, arity: 2 })
    ]

functionLookup :: Operator -> (List UnwrappedQuantity -> UnwrappedQuantity)
functionLookup op =
  _.function
    $ unsafePartial
    $ fromJust
    $ find (\r -> r.token == op) operatorsList

compute' :: Store -> Expression -> Expression
compute' store (Exec op xs) =
  let
    xs' :: List Expression
    xs' = map (compute store) xs
  in
    if all
      ( \x -> case x of
          Quantity _ _ -> true
          _ -> false
      )
      xs' then
      let
        computed_args :: List UnwrappedQuantity
        computed_args = map (unsafePartial \(Quantity a b) -> UnwrappedQuantity a b) xs'
      in
        wrapQuantity $ (functionLookup op) computed_args
    else
      Exec op xs'

compute' store (Var v) = fromMaybe (Var v) (lookup v store)

compute' _ (Quantity v e) = (Quantity v e)

compute :: Store -> Expression -> Expression
compute s e = (compute' s) $ transform e

runCommand :: Store -> Command -> Tuple Store Expression
runCommand s (E e) = Tuple s (compute s e)

runCommand s (D (Declaration variable_name e)) =
  let
    evaledE = compute s e
  in
    Tuple (insert variable_name evaledE s) evaledE

run :: Commands -> List Expression
run commands =
  reverse $ snd
    $ foldl
        ( \(Tuple s es) c ->
            let
              results = runCommand s c
            in
              Tuple (fst results) ((snd results) : es)
        )
        (Tuple constants Nil)
        commands
