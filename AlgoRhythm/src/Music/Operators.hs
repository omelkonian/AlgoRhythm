{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators      #-}
module Music.Operators
       ( (#), (<#)
       , (<|), (<||), (%>)
       , (=|), (+|)
       , (<:)
       , (~~)
       ) where

import Music.Types
import Music.Utilities

-- | Operator precedence.
infix  9 #, ~~
infix  8 <:
infix  7 <|
infix  6 =|, +|
infixl 5 <#, <||, %>

-- Constructors.
(~~) :: Duration -> Music a
(~~) = Rest

(#) :: PitchClass -> Octave -> Pitch
pc # n = (pc, n)

(<#) :: [PitchClass] -> Octave -> [Pitch]
pcs <# n = (# n) <$> pcs

(<:) :: Pitch -> [PitchAttribute] -> FullPitch
p <: attrs = (p, attrs)

(<|) :: a -> Duration -> Music a
(<|) = flip Note

(<||) :: [Pitch] -> Duration -> [Music Pitch]
(<||) sc d = (<| d) <$> sc

(%>) :: Music a -> Duration -> Music a
m %> d = (d~~) :+: m

-- Instantiating chords/scales.
(=|), (+|) :: (Abstract rep a inst) => a -> rep -> inst
(=|) = instantiate
(+|) = instantiate
