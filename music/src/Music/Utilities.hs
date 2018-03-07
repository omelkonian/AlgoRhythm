{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Music.Utilities
       ( Abstract (..)
       , mode
       , line, chord
       ) where

import           Music.Transformations
import           Music.Types

-- | Represents abstractions of certain music elements.
-- e.g. Abstract AbstractChord Pitch Chord
class Abstract rep  -- type of the abstract representation
               a    -- value needed to instantiate a `rep`
               inst -- instantiated type
               where
  instantiate :: a -> rep -> inst

-- | Covers both 'Chord' and 'Scale'.
instance {-# OVERLAPPABLE #-} (Functor f, Abstract rep a inst) => Abstract (f rep) a (f inst) where
  instantiate a = fmap (instantiate a)

instance {-# OVERLAPS #-} Abstract [Interval] Pitch [Pitch] where
  instantiate p rep = [p ~> i | i <- rep]

instance Abstract rep a inst => Abstract rep (Music a) (Music inst) where
  instantiate ma rep = (`instantiate` rep) <$> ma

-- Aliases.
mode :: Int -> AbstractChord -> AbstractChord
mode = invertN
