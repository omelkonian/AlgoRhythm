{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Music.Transformations
       ( Transposable (..)
       , (~>), (<~)
       , Invertible (..)
       ) where

import           Control.Arrow (first)

import           Music.Types

infixl 5 ~>, <~
(~>), (<~) :: Transposable a => a -> Interval -> a
m ~> n = trans n m
m <~ n = snart n m

-- | Anything that can be transposed with an 'Interval'.
class Transposable a where
  trans :: Interval -> a -> a
  snart :: Interval -> a -> a

instance {-# OVERLAPPABLE #-} BoundEnum a => Transposable a where
  trans i = safeToEnum . (+ fromEnum i) . fromEnum
  snart i = safeToEnum . subtract (fromEnum i) . fromEnum

instance {-# OVERLAPS #-} Transposable a => Transposable (Music a) where
  trans i = fmap (trans i)
  snart i = fmap (snart i)

instance {-# OVERLAPS #-} Transposable FullPitch where
  trans i = first (moveN $ fromEnum i)
  snart i = first (moveN $ -(fromEnum i))

instance {-# OVERLAPS #-} (BoundEnum a) => Num a where
  i + i' = safeToEnum $ fromEnum i + fromEnum i'
  i - i' = safeToEnum $ fromEnum i - fromEnum i'
  i * i' = safeToEnum $ fromEnum i * fromEnum i'
  abs = safeToEnum . abs . fromEnum
  signum = safeToEnum . signum . fromEnum
  fromInteger = safeToEnum . fromInteger

-- Anything that can be inverted.
class Invertible f a where
  invert :: f a -> f a

  invertN :: Int -> f a -> f a
  invertN n xs = iterate invert xs !! (n - 1)

instance (Functor m, Invertible f a) => Invertible m (f a) where
  invert = fmap invert

instance Invertible [] Interval where
  invert (P1:xs) =
    P1 : scanl1 (+) (zipWith (curry distance) xs (tail xs ++ [P1]))
    where distance (i, i') | i' > i = i' - i
                           | otherwise = 12 - i
  invert _ = error "inverting malformed interval description"

-- Anything that can be mirrored.
class Retrogradable f a where
  retro :: f a -> f a

instance (Functor m, Retrogradable f a) => Retrogradable m (f a) where
  retro = fmap retro

instance Retrogradable [] a where
  retro = reverse

instance Retrogradable Music a where
  retro (m :+: m') = retro m' :+: retro m
  retro (m :=: m') = retro m :=: retro m'
  retro m          = m
  -- TODO fill with rests

-- TODO scale durations
