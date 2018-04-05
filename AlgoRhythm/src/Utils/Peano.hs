{-# language GADTs #-}
{-# language DataKinds #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Can be used to encode natural numbers as types.

module Utils.Peano (
    Nat  (..)
  , SNat (..)
  , derivePeanoAliases
  , toInt
  , toNat
) where

import Language.Haskell.TH

-- | Singleton definition for `Nat`
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- | Typelevel Peano numbers.
data Nat = Z     -- ^ Zero
         | S Nat -- ^ Successor
instance Show Nat where
  show = ("D"++) . show . toInt

-- | Derives type aliases D0, D1, ..., DX, where Da is equivalent to the decimal
--   number a, written as a Peano number.
derivePeanoAliases :: Integer -- ^ X, the maximum decimal type alias.
                   -> Q [Dec]
derivePeanoAliases nr = do
  let tAliasNames = map (mkName . ("D"++) . show) [0..nr]
  let ts = zip tAliasNames (tAliases nr)
  mapM (\(n,t) -> tySynD n [] (return t)) ts
  where tAliases n   = reverse (foldr nextIter [ConT (mkName "Z")] [0..n])
        nextIter _ b = (AppT (ConT (mkName "S")) (head b)) : b

-- | Converts a `Nat` to its `Int` representation.
toInt :: Nat -> Int
toInt Z = 0
toInt (S x) = 1 + (toInt x)

-- | Converts an `Int` to its `Nat` representation.
toNat :: Int -> Nat
toNat 0 = Z
toNat n = S (toNat (n-1))
