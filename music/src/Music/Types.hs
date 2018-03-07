{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
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
       , ChordType, ScaleType
         -- * Classes
       , ToMusicCore (..)
       , BoundEnum (..)
       , Transposable (..)
         -- * Operators
       , (%), (##)
       , line, chord
       , (#), (<|), (<||)
       , (~>), (<~), (~~>), (<~~)
       , (=|), (+|)
       , (<:)
       ) where

import           Control.Arrow (first)
import           Data.Default
import           Data.Maybe    (fromJust)
import           GHC.Generics  (Generic)
import           GHC.Real      (Ratio (..), (%))

infix  9 #
infix  8 <:
infix  6 <|, =|, +|
infixl 5 ~>, ~~>, <~, <~~, <||
infixr 4 :+:, :=:

---------------------------------- TYPES ---------------------------------------
data Music a = Music a :+: Music a
             | Music a :=: Music a
             | Note Duration a
             | Rest Duration
             deriving (Eq, Show, Generic)

type Duration = Rational
-- TODO bounded, more sensible construction

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
                    deriving (Eq, Show, Generic)

data Interval = P1 | Mi2 | M2 | Mi3 | M3 | P4 | A4
              | P5 | Mi6 | M6 | Mi7 | M7 | P8
              | Mi9 | M9 | Mi10 | M10 | P11 | A11
              | P12 | Mi13 | M13 | Mi14 | M14 | P15
              deriving (Eq, Show, Generic, Enum, Bounded)

type Chord = [Pitch]
type ChordRep = (Pitch, ChordType)
type ChordType = [Interval]
-- TODO voicings

repToChord :: ChordRep -> Chord
repToChord (p, ct) = [p ~~> i | i <- ct]

type Scale = [Pitch]
type ScaleRep = (Pitch, ScaleType)
type ScaleType = [Interval]
-- TODO Modes

repToScale :: ScaleRep -> Scale
repToScale (p, st) = [p ~~> i | i <- st]

-- Common types of 'Music'.
type Melody = Music Pitch
type Rhythm = Music ()
type Harmony = Music Chord

-- TODO Degrees

-------------------------------- INSTANCES -------------------------------------
instance Functor Music where
  fmap f (m :+: m') = (f <$> m) :+: (f <$> m')
  fmap f (m :=: m') = (f <$> m) :=: (f <$> m')
  fmap f (Note d x) = Note d (f x)
  fmap f (Rest d)   = Rest d

-- TODO Foldable, Traversable, etc...

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

instance ToMusicCore Chord where
  toMusicCore (m :+: m')  = toMusicCore m :+: toMusicCore m'
  toMusicCore (m :=: m')  = toMusicCore m :=: toMusicCore m'
  toMusicCore (Note d ps) = toMusicCore $ foldl1 (:=:) $ map (Note d) ps
  toMusicCore (Rest d)    = Rest d

-- Default values.
instance Default PitchClass where
  def = C
instance Default Octave where
  def = Oct4

-- Bounded enumeration of 'Music' datatypes.
instance Enum Pitch where
  toEnum n = (safeToEnum pc, safeToEnum oct)
    where (oct, pc) = n `divMod` 12

  fromEnum (pc, oct) = 12 * fromEnum oct + fromEnum pc

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

----------------------------- Transformations ----------------------------------

-- | A 'Transposable' datatype can be transposed with an 'Interval'.
class Transposable a where
  trans :: Interval -> a -> a
  snart :: Interval -> a -> a

  transM :: Interval -> Music a -> Music a
  transM i = fmap (trans i)
  snartM :: Interval -> Music a -> Music a
  snartM i = fmap (snart i)

instance BoundEnum a => Transposable a where
  trans i = safeToEnum . (+ fromEnum i) . fromEnum
  snart i = safeToEnum . subtract (fromEnum i) . fromEnum

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

-- TODO more trasformations (retro, invert, time ops, etc...)

-------------------------------- OPERATORS -------------------------------------
line, chord :: [Music a] -> Music a
line = foldr1 (:+:)
chord = foldr1 (:=:)

(#) :: PitchClass -> Octave -> Pitch
pc # n = (pc, n)

(<:) :: Pitch -> [PitchAttribute] -> FullPitch
p <: attrs = (p, attrs)

(<|) :: a -> Duration -> Music a
(<|) = flip Note

(<||) :: [Pitch] -> Duration -> [Music Pitch]
(<||) sc d = (<| d) <$> sc

(=|) :: Pitch -> ChordType -> Chord
p =| ct = repToChord (p, ct)

(+|) :: Pitch -> ScaleType -> Scale
p +| st = repToScale (p, st)

(~>), (<~) :: Transposable a => Music a -> Interval -> Music a
m ~> n = transM n m
m <~ n = snartM n m

(~~>), (<~~) :: Transposable a => a -> Interval -> a
m ~~> n = trans n m
m <~~ n = snart n m

-- | Repeat a piece of music.
(##) :: Int -> Music a -> Music a
n ## m | n <= 0    = Rest 0
       | otherwise = m :+: ((n-1) ## m)
