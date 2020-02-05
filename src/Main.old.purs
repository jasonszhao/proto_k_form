module Old where

-- import Prelude

-- import Effect (Effect)
-- import Effect.Console (log)

-- import Math as Math
-- import Data.Foldable (foldl)




-- main :: Effect Unit
-- main = do
--   log "🍝"



-- newtype Value = Value Number
-- newtype AbsError = Abs Number
-- newtype RelError = Rel Number
-- data Quantity = Quantity Value AbsError

-- instance showRelError :: Show RelError where
--   show (Rel x) = show x

-- class Unwrap a where
--   unwrap :: a -> Number

-- class Wrap a where
--   wrap :: Number -> a

-- class Wrapped a

-- instance wrappedRelError :: Wrapped RelError

-- instance unwrapRelError :: Unwrap RelError where
--   unwrap (Rel x) = x



-- --maybe quanitty should also be a typeclass, so
-- -- so we could simply implement "methods" on it

-- rel :: Quantity -> RelError
-- rel (Quantity (Value v) (Abs e)) = Rel (Math.abs (e / v))


-- abs :: Value -> RelError -> AbsError 
-- abs (Value v) (Rel e) = Abs $ Math.abs (e * v)

-- mathSum :: Array Number -> Number
-- mathSum xs = foldl (+) 0.0 xs

-- mathProduct :: Array Number -> Number
-- mathProduct xs = foldl (*) 1.0 xs

-- mathHypot :: Array Number -> Number
-- mathHypot xs = Math.sqrt $ mathSum $ map (\x -> x*x) xs


-- kenyonForm :: Quantity -> Array String  -- not really
-- kenyonForm (Quantity (Value v) (Abs e)) = 
--   [ show v 
--   , show e
--   , show $ rel (Quantity (Value v) (Abs e))]

-- add :: Array Quantity -> Quantity
-- add xs = 
--   let 
--     vs = map (\(Quantity (Value v) _) -> v) xs
--     es = map  (\(Quantity _ (Abs e)) -> e) xs
--   in Quantity (Value (mathSum vs)) (Abs (mathHypot es))

-- multiply :: Array Quantity -> Quantity
-- multiply xs = 
--   let 
--     vs = map (\(Quantity (Value v) _) -> v) xs
--     es = map (rel >>> (\(Rel e) -> e)) xs
--   in Quantity (Value (mathSum vs)) (Abs (mathHypot es))


-- exp ::  Number -> Quantity -> Quantity
-- exp n (Quantity (Value v) (Abs e)) = 
--   let 
--     val = Value $ Math.pow v e

--     relErr :: RelError
--     relErr = Rel $ n * unwrap (rel (Quantity (Value v) (Abs e)))
--   in 
--     Quantity val (abs val relErr)




-- scale :: Number -> Quantity -> Quantity
-- scale a (Quantity (Value v) (Abs e)) = (Quantity (Value (a * v)) (Abs (a * e)))



-- -- UI: 
--  -- input
--   -- result

