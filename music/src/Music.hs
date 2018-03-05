{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
module Music ( -- Datatypes
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
               -- Classes
             , ToMusicCore (..)
             , BoundEnum (..)
             , Transposable (..)
               -- Operators
             , (%)
             , line
             , chord
             , (<|)
             , (~>), (<~)
             , (<@)
             , (<:)
             ) where

import           Control.Arrow (first)
import           Data.Default
import           Data.Maybe    (fromJust)
import           GHC.Generics  (Generic)
import           GHC.Real      (Ratio (..), (%))


-------------------------------- BASE TYPES ------------------------------------
data Music a = Music a :+: Music a
             | Music a :=: Music a
             | Note Duration a
             | Rest Duration
             deriving (Eq, Show, Generic)
infix 3 :+: -- sequential
      , :=: -- parallel

type Duration = Rational
-- instance Bounded Duration where
--   minBound =

type FullPitch = (Pitch, [PitchAttribute])

type Pitch = (PitchClass, Octave)

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
                  deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data Octave = Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6
              deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data PitchAttribute = Dynamic Dynamic
                    | Articulation Articulation
                    deriving (Eq, Show, Generic)
                    -- TODO GroupedArticulation (e.g. slur, legato)

data Dynamic = PPPPP | PPPP | PPP | PP | P | MP | MF | F_ | FF | FFF | FFFF
               deriving (Eq, Show, Generic, Enum, Bounded, Ord)

data Articulation = Staccato | Staccatissimo | Marcato | Tenuto
                    deriving (Eq, Show, Generic)

-------------------------------- INSTANCES -------------------------------------
instance Functor Music where
  fmap f (m :+: m') = (f <$> m) :+: (f <$> m')
  fmap f (m :=: m') = f <$> m :=: f <$> m'
  fmap f (Note d x) = Note d (f x)
  fmap f (Rest d)   = Rest d

-- | Core instance.
--   All user-defined abstractions must convert to this core representation.
type MusicCore = Music FullPitch

class ToMusicCore a where
  toMusicCore :: Music a -> MusicCore

-- | 'FullPitch' is defined as the core music type,
--   so this instance doesn't change anything.
instance ToMusicCore FullPitch where
  toMusicCore = id

-- | Other Music instances.
type Melody = Music Pitch
instance ToMusicCore Pitch where
  toMusicCore = fmap (\p -> (p, def))

type Rhythm = Music ()
instance ToMusicCore Duration where
  toMusicCore = toMusicCore . fmap (const (def :: Pitch))

type Chord = [Pitch]
type Harmony = Music Chord
instance ToMusicCore Chord where
  toMusicCore (m :+: m')  = toMusicCore m :+: toMusicCore m'
  toMusicCore (m :=: m')  = toMusicCore m :=: toMusicCore m'
  toMusicCore (Note d ps) = toMusicCore $ foldl1 (:=:) $ map (Note d) ps
  toMusicCore (Rest d)    = Rest d

-- | 'Transposable' instances.
class (Enum a, Bounded a) => BoundEnum a where
  -- | Safely convert from 'Int', respecting bounds.
  safeToEnum :: Int -> a
  safeToEnum = toEnum . min top . max bottom
    where top = fromEnum (maxBound :: a)
          bottom = fromEnum (minBound :: a)

  -- | Get next value or min/max if out-of-bounds.
  next ::  a -> a
  next = safeToEnum . (+1) . fromEnum

  -- | Get previous value or min/max if out-of-bounds.
  prev :: a -> a
  prev = safeToEnum . subtract 1 . fromEnum

  moveN :: Int -> a -> a
  moveN n a | n < 0     = iterate prev a !! abs n
            | otherwise = iterate next a !! n

instance (Enum a, Bounded a) => BoundEnum a where

class Transposable a where
  transpose :: Interval -> Music a -> Music a
  transposeDown :: Interval -> Music a -> Music a

instance BoundEnum a => Transposable a where
  transpose i m = (safeToEnum . (+ fromEnum i) . fromEnum) <$> m
  transposeDown i m = (safeToEnum . subtract (fromEnum i) . fromEnum) <$> m

instance {-# OVERLAPS #-} Transposable FullPitch where
  transpose i m = first (moveN $ fromEnum i) <$> m
  transposeDown i m = first (moveN $ -(fromEnum i)) <$> m

instance {-# OVERLAPS #-} (BoundEnum a) => Num a where
  i + i' = safeToEnum $ fromEnum i + fromEnum i'
  i - i' = safeToEnum $ fromEnum i - fromEnum i'
  i * i' = safeToEnum $ fromEnum i * fromEnum i'
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  fromInteger = toEnum . fromInteger

-- | 'Default' instances.
instance Default PitchClass where
  def = C
instance Default Octave where
  def = Oct4

-- | Absolute pitches.
type AbsPitch = Int

instance Enum Pitch where
  toEnum n = (pc, toEnum $ bound oct)
    where (oct, i) = n `divMod` 12
          pc = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! i
          bound = min (fromEnum (maxBound :: Octave)) .
                  max (fromEnum (minBound :: Octave))

  fromEnum (pc, oct) = 12 * fromEnum oct + fromEnum pc

-- | Intervals.
data Interval = P1 | Min2 | M2 | Min3 | M3 | P4 | D5
              | P5 | Min6 | M6 | Min7 | M7 | P8
              | Min9 | M9 | Min10 | M10 | P11 | A11
              | P12 | Min13 | M13 | Min14 | M14 | P15
              deriving (Eq, Show, Generic, Enum, Bounded)

-- | TODO Operators / smart constructors.
line, chord :: [Music a] -> Music a
line = foldr1 (:+:)
chord = foldr1 (:=:)

infix 5 <@
(<@) :: PitchClass -> Int -> Pitch
pc <@ n = (pc, toEnum n)

infix 4 <:
(<:) :: Pitch -> [PitchAttribute] -> FullPitch
p <: attrs = (p, attrs)

infix 3 <|
(<|) :: a -> Duration -> Music a
(<|) = flip Note

infix 2 ~>
(~>) :: Transposable a => Music a -> Interval -> Music a
m ~> n = transpose n m

infix 1 <~
(<~) :: Transposable a => Music a -> Interval -> Music a
m <~ n = transposeDown n m

-- | TODO Degrees

-- | TODO Key signatures

-- | TODO Scales

-- | TODO Time signatures

-- | TODO Chords/Chord types
