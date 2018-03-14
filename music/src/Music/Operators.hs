{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Music.Operators
       ( (#), (<#)
       , (<|), (<||)
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
infixl 5 <#, <||

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

-- Instantiating chords/scales.
(=|), (+|) :: (Abstract rep Pitch inst) => Pitch -> rep -> inst
(=|) = instantiate
(+|) = instantiate
