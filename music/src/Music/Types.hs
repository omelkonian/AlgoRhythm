{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Music.Types
       ( -- * Types
         Music (..)
       , Duration
       , FullPitch
       , Pitch
       , PitchClass (..)
       , Octave (..)
       , PitchAttribute (..)
       , Dynamic (..)
       , Interval (..)
       , Articulation (..)
       , MusicCore, AbsPitch
       , Melody, Rhythm, Harmony
       , Chord, SemiChord, AbstractChord
       , Scale, SemiScale, AbstractScale
         -- * Classes
       , ToMusicCore (..)
       , BoundEnum (..)
         -- * Shorthands
       , (<$$>), (<$$$>)
       , (%), Default(..)
       , line, chord, scale, parallel, voices
       , flatten, harmonyToMelody
       , absPitch, pitch
       ) where

import Data.Default (Default (..))
import Data.Monoid  ((<>))
import GHC.Generics (Generic)
import GHC.Real     ((%))

-- | Operator precedence.
infixr 4 :+:, :=:, <$$>

---------------------------------- TYPES ---------------------------------------
data Music a = Music a :+: Music a
             | Music a :=: Music a
             | Note Duration a
             | Rest Duration
             deriving (Eq, Show, Generic)

type Duration = Rational

type FullPitch = (Pitch, [PitchAttribute])

type Pitch = (PitchClass, Octave)

type AbsPitch = Int

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
                  deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data Octave = Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6
              deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data PitchAttribute = Dynamic Dynamic
                    | Articulation Articulation
                    deriving (Eq, Show, Generic)

data Dynamic = PPPPP | PPPP | PPP | PP | P | MP | MF | F_ | FF | FFF | FFFF
               deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data Articulation = Staccato | Staccatissimo | Marcato | Tenuto
                    deriving (Eq, Show, Generic, Enum, Bounded)

data Interval = P1 | Mi2 | M2 | Mi3 | M3 | P4 | A4
              | P5 | Mi6 | M6 | Mi7 | M7 | P8
              | Mi9 | M9 | A9 | M10 | P11 | A11
              | P12 | Mi13 | M13 | Mi14 | M14 | P15
              deriving (Eq, Show, Generic, Enum, Bounded, Ord)

type Chord = [Pitch]
type Scale = [Pitch]
type SemiChord = [PitchClass]
type SemiScale = [PitchClass]
type AbstractChord = [Interval]
type AbstractScale = [Interval]

-- Common types of 'Music'.
type Melody = Music Pitch
type Rhythm = Music ()
type Harmony = Music Chord

-------------------------------- INSTANCES -------------------------------------
instance Functor Music where
  fmap f (m :+: m') = (f <$> m) :+: (f <$> m')
  fmap f (m :=: m') = (f <$> m) :=: (f <$> m')
  fmap f (Note d x) = Note d (f x)
  fmap _ (Rest d)   = Rest d

-- For mapping over durations.
(<$$>) :: (Duration -> Duration) -> Music a -> Music a
f <$$> (m :+: m') = (f <$$> m) :+: (f <$$> m')
f <$$> (m :=: m') = (f <$$> m) :=: (f <$$> m')
f <$$> (Note d x) = Note (f d) x
f <$$> (Rest d)   = Rest (f d)

-- For mapping primitive musical elements (i.e. 'Note' and 'Rest').
(<$$$>) :: (Music a -> Music b) -> Music a -> Music b
f <$$$> (m :+: m') = (f <$$$> m) :+: (f <$$$> m')
f <$$$> (m :=: m') = (f <$$$> m) :=: (f <$$$> m')
f <$$$> m = f m

instance Foldable Music where
  foldMap f (m :+: m') = foldMap f m <> foldMap f m'
  foldMap f (m :=: _)  = foldMap f m
  foldMap f (Note _ a) = f a
  foldMap _ _          = mempty

instance Enum FullPitch where
  fromEnum ((pc,oct),_) = (fromEnum oct) *mOct + (fromEnum pc)
  toEnum   i            = ((toEnum (i `mod` mOct), toEnum (i `div` mOct)),[])
mOct :: Int
mOct = fromEnum (maxBound :: Octave)

-- | Core 'Music' datatype.
type MusicCore = Music FullPitch

-- | To allow playback, exporting to MIDI and rendering scores, all user-defined
-- abstractions must be convertible to 'MusicCore'.
class ToMusicCore a where
  toMusicCore :: Music a -> MusicCore

-- | 'FullPitch' is defined as the core music type,
-- so this instance doesn't change anything.
instance ToMusicCore FullPitch where
  toMusicCore = id

instance ToMusicCore Pitch where
  toMusicCore = fmap (\p -> (p, def))

instance ToMusicCore AbsPitch where
  toMusicCore = toMusicCore . fmap (\i -> (toEnum i :: Pitch, def :: [PitchAttribute]))

instance ToMusicCore Duration where
  toMusicCore = toMusicCore . fmap (const (def :: Pitch))

instance ToMusicCore PitchClass where
  toMusicCore = fmap (\pc -> ((pc, def), def))

instance ToMusicCore a => ToMusicCore [a] where
  toMusicCore (m :+: m')  = toMusicCore m :+: toMusicCore m'
  toMusicCore (m :=: m')  = toMusicCore m :=: toMusicCore m'
  toMusicCore (Note d ps) = toMusicCore $ chord $ Note d <$> ps
  toMusicCore (Rest d)    = Rest d

-- Default values.
instance Default PitchClass where
  def = C
instance Default Octave where
  def = Oct4
instance {-# OVERLAPS #-} Default Duration where
  def = 1

-- Bounded enumeration of 'Music' datatypes.
instance Enum Pitch where
  toEnum n = (safeToEnum pc, safeToEnum oct)
    where (oct, pc) = n `divMod` 12

  fromEnum (pc, oct) = 12 * fromEnum oct + fromEnum pc

class (Eq a, Enum a, Bounded a) => BoundEnum a where
  -- | Safely convert from 'Int', respecting bounds.
  safeToEnum :: Int -> a
  safeToEnum = toEnum . min top . max bottom
    where top = fromEnum (maxBound :: a)
          bottom = fromEnum (minBound :: a)

  -- | Get next value or min/max if out-of-bounds.
  next ::  a -> a
  next = safeToEnum . (+ 1) . fromEnum

  -- | Get previous value or min/max if out-of-bounds.
  prev :: a -> a
  prev = safeToEnum . subtract 1 . fromEnum

  -- | Move n-times forward.
  moveN :: Int -> a -> a
  moveN n a | n < 0     = iterate prev a !! abs n
            | otherwise = iterate next a !! n

  -- | Variant of 'prev' that cycles forth to the maximum.
  prev_ :: Eq a => a -> a
  prev_ a | a == minBound = maxBound
          | otherwise = prev a

  -- | Variant of 'next' that cycles back to the minimum.
  next_ :: Eq a => a -> a
  next_ a | a == maxBound = minBound
          | otherwise = next a

  -- | Cycle n-times forward.
  moveN_ :: Eq a => Int -> a -> a
  moveN_ n a | n < 0     = iterate prev_ a !! abs n
             | otherwise = iterate next_ a !! n

instance (Eq a, Enum a, Bounded a) => BoundEnum a where

-- Useful shorthands.
line, chord, scale, parallel :: [Music a] -> Music a
line = foldr1 (:+:)
chord = foldr1 (:=:)
scale = line
parallel = chord

-- TODO handle deeper nesting
voices :: Music a -> [Music a]
voices (m :=: m') = m : voices m'
voices m = [m]

flatten :: Music (Music a) -> Music a
flatten (m :+: m') = flatten m :+: flatten m'
flatten (m :=: m') = flatten m :=: flatten m'
flatten (Note _ m) = m
flatten (Rest d)   = Rest d

harmonyToMelody :: Harmony -> Melody
harmonyToMelody (m :+: m')  = harmonyToMelody m :+: harmonyToMelody m'
harmonyToMelody (m :=: m')  = harmonyToMelody m :=: harmonyToMelody m'
harmonyToMelody (Note d xs) = chord (Note d <$> xs)
harmonyToMelody (Rest d)    = Rest d

absPitch :: Pitch -> AbsPitch
absPitch = fromEnum
pitch :: AbsPitch -> Pitch
pitch = toEnum
