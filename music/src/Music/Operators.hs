{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Music.Operators
       ( (%), (##)
       , (#), (<|), (<||), (.-)
       , (=|), (+|)
       , (<:)
       , (~>), (<~)
       ) where

import           Data.Ratio            ((%))

import           Music.Transformations
import           Music.Types
import           Music.Utilities

infix  9 #
infix  8 <:
infix  7 <|
infix  6 =|, +|
infixl 5 <||

-- Constructors.
(.-) :: Duration -> Music a
(.-) = Rest

(#) :: PitchClass -> Octave -> Pitch
pc # n = (pc, n)

(<:) :: Pitch -> [PitchAttribute] -> FullPitch
p <: attrs = (p, attrs)

(<|) :: a -> Duration -> Music a
(<|) = flip Note

(<||) :: [Pitch] -> Duration -> [Music Pitch]
(<||) sc d = (<| d) <$> sc

-- Instantiating chords/scales.
(=|), (+|) :: (Abstract rep Pitch inst) => Pitch -> rep -> inst
(=|) = instantiate
(+|) = instantiate

-- Repeat a piece of music.
(##) :: Int -> Music a -> Music a
n ## m | n <= 0    = Rest 0
       | otherwise = m :+: ((n-1) ## m)
