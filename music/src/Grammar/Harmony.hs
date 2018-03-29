{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.Harmony
       ( HarmonyConfig (..), defHarmonyConfig
       , harmony, interpret
       , Degree (..), Modulation (..)
       ) where

import Grammar.Types
import Grammar.Utilities
import Music

-- | Terminal symbol that represents scale degrees.
data Degree = I | II | III | IV | V | VI | VII
              deriving (Eq, Show, Enum, Bounded)

-- | Auxiliary wrapper for modulating keys.
newtype Modulation = Modulation Interval deriving (Eq, Show)

-- | Grammar for harmonic structures.
harmony :: Grammar Modulation Degree
harmony =
  [ -- Turn-arounds
    (I, 8, (> wn)) :-> \t -> Let (I%:t/2) (\x -> x :-: x)
  , (I, 2, (> wn)) :-> \t -> I%:t/2 :-: I%:t/2
  , (I, 6, (> hn) /\ (<= wn)) :-> \t -> II%:t/4 :-: V%:t/4 :-: I%:t/2
  , (I, 2, (> hn) /\ (<= wn)) :-> \t -> V%:t/2 :-: I%:t/2
  , (I, 2) -|| (<= wn)
    -- TODO ++

    -- Modulations
  , (V, 5, (> hn)) :-> \t -> Modulation P5 $: I%:t
  , V -| 3
  -- , (II, 2, (> hn)) :-> \t -> Modulation M2 |$: I%:t
  -- , II -| 8
    -- TODO ++

    -- Tritone substitution
  -- , (V, 2, (> hn)) :-> \t -> Let (V%:t/4 :-: Modulation A4 |$: V%:t/4) (\x -> x :-: x)
    -- TODO ++
  ]

-- | Expands modulations and intreprets degrees to chords.
instance Expand HarmonyConfig Degree Modulation SemiChord where
  expand conf (m :-: m') = (:-:) <$> expand conf m <*> expand conf m'
  expand conf (Let x f)  = f <$> expand conf x
  expand conf (Aux _ (Modulation itv) t) =
    expand (conf {basePc = basePc conf ~~> itv}) t
  expand conf (Prim (a, t)) = do
    ch <- conf `interpret` a
    return $ Prim (ch, t)

-- | Interpret a degree as a 'SemiChord' on a given harmonic context.
interpret :: HarmonyConfig -> Degree -> IO SemiChord
interpret config degree = choose options
  where tonic = basePc config +| baseScale config :: SemiScale
        tone = tonic !! fromEnum degree
        options = [ (w, ch)
                  | (w, chordType) <- chords config
                  , let ch = tone =| chordType
                  , all (`elem` tonic) ch
                  ]

-- | Configuration for harmony.
data HarmonyConfig = HarmonyConfig
  { basePc    :: PitchClass
  , baseOct   :: Octave
  , baseScale :: AbstractScale
  , chords    :: [(Weight, AbstractChord)]
  }

defHarmonyConfig :: HarmonyConfig
defHarmonyConfig = HarmonyConfig
  { basePc  = def
  , baseOct = def
  , baseScale = major
  , chords = equally allChords
  }
