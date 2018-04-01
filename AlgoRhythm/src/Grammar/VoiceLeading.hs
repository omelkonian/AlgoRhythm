{-# LANGUAGE ImplicitParams #-}
module Grammar.VoiceLeading (voiceLead) where

import Grammar.Utilities
import Grammar.Harmony
import Music

-- | Produce concrete chords out of a harmonic structure.
voiceLead :: (?harmonyConfig :: HarmonyConfig) => Music SemiChord -> IO (Music Chord)
voiceLead m' = do
  vl <- foldl f (pure [(initC, t)]) ms
  return $ fromList vl
  where
    initC = toBaseChord c
    ((c, t) : ms) = toList m'
    f :: IO [(Chord, Duration)] -> (SemiChord, Duration) -> IO [(Chord, Duration)]
    f cs' (sc, d) = do
      cs <- cs'
      c' <- smoothTransition initC (fst $ last cs) sc
      return $ cs ++ [(c', d)]

-- | Get a basic voicing of a chord in a given octave.
toBaseChord :: (?harmonyConfig :: HarmonyConfig) => SemiChord -> Chord
toBaseChord = fmap (\pc -> (pc, baseOct ?harmonyConfig))

-- | Get all inversions of +-1 octave.
allInversions :: (?harmonyConfig :: HarmonyConfig) => SemiChord -> [Chord]
allInversions c =
  let initC = toBaseChord c
      n = length c
      invs ch = take n $ iterate invert ch
  in invs (initC ~> P8) ++ invs initC ++ invs (initC <~ P8)

-- | Smooth voice-leading from one chord to another (i.e. minimal pitch distance).
smoothTransition :: (?harmonyConfig :: HarmonyConfig) => Chord -> Chord -> SemiChord -> IO Chord
smoothTransition initC prevC curC =
  chooseWith setWeight (allInversions curC)
  where
    -- | Set probability weight based on (inverse) pitch distance.
    setWeight :: Chord -> Double
    setWeight c = 1.0 / fromIntegral (2 * chordDistance initC c + chordDistance prevC c)
