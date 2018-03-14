{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Music.Transformations
       ( Transposable (..)
       , Invertible (..)
       , Retrogradable (..)
       , Repeatable (..)
       , Scalable (..)
       , musicToList, listToMusic
       , normalize
       ) where

import Control.Arrow (first)
import Data.Maybe    (catMaybes)

import Music.Types

-- | Operator precedence.
infixl 5 ~>, <~
infix  3 *~
infix  2 ##

-- | Anything that can be transposed with an 'Interval'.
class Transposable a where
  trans, trans_, snart, snart_ :: Interval -> a -> a
  (~>), (<~), (~~>), (<~~) :: a -> Interval -> a
  (~>) = flip trans ; (<~) = flip snart ; (~~>) = flip trans_ ; (<~~) = flip snart_

instance {-# OVERLAPPABLE #-} BoundEnum a => Transposable a where
  trans  = moveN . fromEnum
  snart  = moveN . negate . fromEnum
  trans_ = moveN_ . fromEnum
  snart_ = moveN_ . negate . fromEnum

instance {-# OVERLAPS #-} Transposable a => Transposable (Music a) where
  trans  = fmap . trans
  snart  = fmap . snart
  trans_ = fmap . trans_
  snart_ = fmap . snart_

instance {-# OVERLAPS #-} Transposable a => Transposable [a] where
  trans  = fmap . trans
  snart  = fmap . snart
  trans_ = fmap . trans_
  snart_ = fmap . snart_

instance {-# OVERLAPS #-} Transposable FullPitch where
  trans  i = first (moveN  $ fromEnum i)
  snart  i = first (moveN  $ -(fromEnum i))
  trans_ i = first (moveN_ $ fromEnum i)
  snart_ i = first (moveN_ $ -(fromEnum i))

instance {-# OVERLAPS #-} (Enum a, BoundEnum a) => Num a where
  i + i' = moveN (fromEnum i') i
  i - i' = moveN (- (fromEnum i')) i
  i * i' = moveN (fromEnum i * (fromEnum i' - 1)) i
  abs = safeToEnum . abs . fromEnum
  signum = safeToEnum . signum . fromEnum
  fromInteger = safeToEnum . fromInteger

-- Anything that can be inverted.
class Invertible f a where
  invert :: f a -> f a

  invertN :: Int -> f a -> f a
  invertN n xs = iterate invert xs !! (n - 1)

instance Invertible [] a => Invertible [] (Maybe a) where
  invert ms = go ms (invert $ catMaybes ms)
    where go (x:xs) (y:ys) = case x of Just _  -> Just y : go xs ys
                                       Nothing -> Nothing : go xs ys
          go _ _ = []

instance Invertible [] a => Invertible [] (a, b) where
  invert = uncurry zip . first invert . unzip

instance (Show a, Invertible [] a) => Invertible Music a where
  invert = listToMusic . invert . musicToList

instance Invertible [] Interval where
  invert (P1:xs) =
    P1 : scanl1 (+) (zipWith (curry distance) xs (tail xs ++ [P1]))
    where distance (i, i') | i' > i = i' - i
                           | otherwise = 12 - i
  invert _ = error "inverting malformed interval description"

instance Invertible [] AbsPitch where
  invert = fmap negate

instance {-# OVERLAPS #-} Invertible [] Pitch where
  invert [] = []
  invert ps = pitch <$> aps'
    where aps' = (+ pivot) <$> inverted
          inverted = invert distances
          distances = (\ap -> ap - pivot) <$> aps
          aps = absPitch <$> ps
          pivot = head aps

-- Anything that can be mirrored.
class Retrogradable f a where
  (><) :: f a -> f a

instance Retrogradable [] a where
  (><) = reverse

instance Retrogradable Music a where
  (><) = normalize . retro
    where retro (m :+: m') = (m'><) :+: (m><)
          retro (m :=: m') = (m><) :=: (m'><)
          retro m          = m

-- | Anything that can be scaled up/down.
class Scalable a where
  (*~) :: Rational -> a -> a

instance Scalable Duration where
  (*~) n d = d / n

instance Scalable a => Scalable [a] where
  (*~) n xs = (n *~) <$> xs

instance Scalable (Music a) where
  (*~) n m = (n *~) <$$> m

-- | Anything that can be repeated a number of times.
class Repeatable a where
  (##) :: Int -> a -> a

instance Repeatable (Music a) where
  n ## m | n == 1    = m
         | otherwise = m :+: ((n-1) ## m)

-- | Normalize nested application of sequential composition.
normalize :: Music a -> Music a
normalize (m :+: m') = listToMusic $ musicToList m ++ musicToList m'
normalize (m :=: m') = normalize m :=: normalize m'
normalize m          = m

-- | Conversion to/from 'List'.
musicToList :: Music a -> [(Maybe a, Duration)]
musicToList (m :+: m') = musicToList m ++ musicToList m'
musicToList (m :=: _)  = musicToList m
musicToList (Note d a) = [(Just a, d)]
musicToList (Rest d)   = [(Nothing, d)]

listToMusic :: [(Maybe a, Duration)] -> Music a
listToMusic = line . map (uncurry $ \m d ->
  case m of Nothing -> Rest d
            Just a  -> Note d a)
