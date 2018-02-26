{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Music where

import           Data.Default
import           GHC.Generics (Generic)
import           GHC.Real     (Ratio (..))

data Music a = Music a :+: Music a
             | Music a :=: Music a
             | Note Duration a
             | Rest Duration
             deriving (Eq, Show, Generic)
infix 3 :+: -- sequential
      , :=: -- parallel

type Duration = Rational

type FullPitch = (Pitch, [PitchAttribute])

data Pitch = PitchClass :@: Octave
             deriving (Eq, Show, Generic)
infix 4 :@: -- notes

data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
                 deriving (Eq, Show, Generic)

type Octave = Int

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

-- | Default instances.
instance Default Pitch where
  def = def :@: def
instance Default PitchClass where
  def = C

-- | Other Music instances.
type Melody = Music Pitch
instance ToMusicCore Pitch where
  toMusicCore = fmap (\p -> (p, def))

type Rhythm = Music ()
instance ToMusicCore Duration where
  toMusicCore = toMusicCore . fmap (const (def :: Pitch))

type Harmony = Music [Pitch]
instance ToMusicCore [Pitch] where
  toMusicCore (m :+: m')  = toMusicCore m :+: toMusicCore m'
  toMusicCore (m :=: m')  = toMusicCore m :=: toMusicCore m'
  toMusicCore (Note d ps) = toMusicCore $ foldl1 (:=:) $ map (Note d) ps
  toMusicCore (Rest d)    = Rest d

-- | TODO Smart constructors.
