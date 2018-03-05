{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Music where

import           Control.Arrow (first)
import           Data.Default
import           Data.Maybe    (fromJust)
import           GHC.Generics  (Generic)
import           GHC.Real      (Ratio (..))

import           Debug.Trace

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

data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
                 deriving (Eq, Show, Generic, Enum)

data Octave = Oct0 | Oct1 | Oct2 | Oct3 | Oct4 | Oct5 | Oct6
              deriving (Eq, Show, Generic, Enum, Bounded)

data PitchAttribute = Dynamics Dynamics
                    | Articulation Articulation
                    deriving (Eq, Show, Generic)
                    -- TODO GroupedArticulation (e.g. slur, legato)

data Dynamics = PPPPP | PPPP | PPP | PP | P | MP | MF | F_ | FF | FFF | FFFF
                deriving (Eq, Show, Generic)

data Articulation = Staccato | Staccatissimo | Martellato | Marcato | Tenuto
                    deriving (Eq, Show, Generic)

-- | Music instances.
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

-- | Transposable instances.
class Transposable a where
  transpose :: Int -> Music a -> Music a

instance Transposable Pitch where
  transpose n m = transposePitch n <$> m

instance Transposable FullPitch where
  transpose n fp = first (transposePitch n) <$> fp

instance Transposable Chord where
  transpose n (m :+: m')  = transpose n m :+: transpose n m'
  transpose n (m :=: m')  = transpose n m :=: transpose n m'
  transpose n (Note d ps) = Note d (transposePitch n <$> ps)
  transpose n (Rest d)    = Rest d

transposePitch :: Int -> Pitch -> Pitch
transposePitch n p = iterate succ p !! n

-- | Default instances.
instance Default PitchClass where
  def = C
instance Default Octave where
  def = Oct4

-- | Absolute pitches.
type AbsPitch = Int

instance Enum Pitch where
  toEnum n = (pc, toEnum $ bound (oct - 1))
    where (oct, i) = n `divMod` 12
          pc = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! i
          bound = min (fromEnum (maxBound :: Octave)) .
                  max (fromEnum (minBound :: Octave))

  fromEnum (pc, oct) = 12 * (fromEnum oct + 1) + fromJust (lookup pc pcInts)
    where pcInts = [ (Cff, -2), (Cf, -1), (C, 0), (Cs, 1), (Css, 2), (Dff, 0)
                   , (Df, 1), (D, 2), (Ds, 3), (Dss, 4), (Eff, 2), (Ef, 3)
                   , (E, 4), (Es, 5), (Ess, 6), (Gff, 5), (Gf, 6), (G, 7)
                   , (Gs, 8), (Gss, 9), (Fff, 3), (Ff, 4), (F, 5), (Fs, 6)
                   , (Fss, 7), (Aff, 7), (Af, 8), (A, 9), (As, 10), (Ass, 11)
                   , (Bff, 9), (Bf, 10), (B, 11), (Bs, 12), (Bss, 13)
                   ]

-- | TODO Smart constructors.
line, chord :: [Music a] -> Music a
line = foldr1 (:+:)
chord = foldr1 (:=:)

(<|) :: a -> Duration -> Music a
(<|) = flip Note

(~|) :: Transposable a => Music a -> Interval -> Music a
m ~| n = transpose (fromEnum n) m

-- infix 4 <@
(<@) :: PitchClass -> Int -> Pitch
pc <@ n = (pc, toEnum n)

-- | TODO Operations

-- | TODO Intervals
data Interval = P1 | Min2 | M2 | Min3 | M3 | P4 | D5
              | P5 | Min6 | M6 | Min7 | M7 | P8
              deriving (Eq, Show, Generic, Enum)

-- | TODO Degrees

-- | TODO Key signatures

-- | TODO Scales

-- | TODO Time signatures

-- | TODO Chords/Chord types
