{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
-- Suppress all unused TH-generated type aliases.
{-# OPTIONS_GHC -fno-warn-unused-top-binds               #-}

module Vec (
    module Vec
  , module Peano
) where

import Prelude hiding (pred)
import Peano
import Data.Monoid ((<>))

-- | Derives type aliases D0, D1, ..., D100, where Da is equivalent to the
--   decimal number a, written as a Peano number. This enables the user to write
-- ` Vec D3 Int`, instead of `Vec ('S ('S ('S 'Z))) Int`.
$(derivePeanoAliases 100)

data Vec n a where
  Nil  :: Vec Z a
  (:.) :: a -> Vec n a -> Vec (S n) a
infixr 4 :.

instance Eq a => Eq (Vec n a) where
  Nil       == Nil       = True
  (x :. xs) == (y :. ys) = x == y && xs == ys

instance Functor (Vec n) where
  fmap _  Nil    = Nil
  fmap f (x:.xs) = f x :. fmap f xs

instance Foldable (Vec n) where
  foldMap _ Nil = mempty
  foldMap f (x:.xs) = f x <> foldMap f xs

instance Show a => Show (Vec n a) where
  show = show . list

list :: Vec n a -> [a]
list = foldr (:) []

vec :: SNat n -> [a] -> Vec n a
vec  SZ     []    = Nil
vec (SS n) (x:xs) = x :. (vec n xs)
vec  _      _     = error "Given SNat is different than the length of the given list."
