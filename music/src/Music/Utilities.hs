{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Music.Utilities
       ( Abstract (..)
       , mode
       , line, chord
       ) where

import Music.Transformations
import Music.Types

-- | Represents abstractions of certain music elements.
-- e.g. Abstract AbstractChord Pitch Chord
class Abstract rep  -- type of the abstract representation
               a    -- value needed to instantiate a `rep`
               inst -- instantiated type
               where
  instantiate :: a -> rep -> inst

-- | Covers both 'Chord' and 'Scale'.
instance Abstract [Interval] Pitch [Pitch] where
  instantiate p rep = [p ~~> i | i <- rep]

instance Abstract [Interval] PitchClass [PitchClass] where
  instantiate p rep = [p ~~> if i - P8 > P1 then i - P8 else i | i <- rep]

instance (Functor f, Abstract rep a inst) => Abstract rep (f a) (f inst) where
  instantiate ma rep = (`instantiate` rep) <$> ma

-- Aliases.
mode :: Int -> AbstractChord -> AbstractChord
mode = invertN
