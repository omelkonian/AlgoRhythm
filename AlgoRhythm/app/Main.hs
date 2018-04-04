{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Dynamics
import Export
import Grammar
import Music
import qualified Generate as Gen

main :: IO ()
main = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = japanese
        , chords  = equally allChords
        }
  let ?midiConfig = defaultMIDIConfig
  let t = 4 * wn
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure

  let melodyConfig = Gen.GenConfig
        { Gen.key                = C
        , Gen.baseScale          = japanese
        , Gen.chords             = harmonicStructure
        , Gen.phraseDistribution = [(1, Gen.High), (1, Gen.Medium), (2, Gen.Low)]
        , Gen.octaveDistribution = [(1, 3), (3, 4), (2, 5)]
        }
  foreground <- Gen.runGenerator () (Gen.diatonicMelody melodyConfig)

  playDev 4 $ 2 ## dyn (toMusicCore background :=: toMusicCore foreground)

-- | Chaos blues. Generates a short music composition using the Chaos function
--   represented in Figure 1 of Chaos Melody Theory by Elaine Walker
--   (http://www.ziaspace.com/elaine/chaos/ChaosMelodyTheory.pdf)
chaosBlues :: Bool -> IO ()
chaosBlues addDyn = do
  m <- Gen.genChaosMusic
  let ?midiConfig = defaultMIDIConfig
  writeToMidiFile "out.ly" m
  playDev 0 (if addDyn then dyn m else (toMusicCore m))

--
jazz :: IO ()
jazz = do
  let ?midiConfig = MIDIConfig (3%4) [AcousticGrandPiano]
  let t = 12 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = D
        , baseOct = Oct4
        , baseScale = dorian
        , chords  = equally [maj, mi, maj7, m7, dim, d7, m7b5]
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure

  let melodyConfig = Gen.GenConfig
        { Gen.key                = D
        , Gen.baseScale          = dorian
        , Gen.chords             = harmonicStructure
        , Gen.phraseDistribution = [(4, Gen.High), (7, Gen.Medium), (2, Gen.Low)]
        , Gen.octaveDistribution = [(2, 3), (7, 4), (4, 5)]
        }
  foreground <- Gen.runGenerator () (Gen.diatonicMelody melodyConfig)

  writeToMidiFile "out.midi" (foreground :=: toMusicCore background)

-- A piece with fast banjo playing
fastBanjo :: IO ()
fastBanjo = do
  let ?midiConfig = MIDIConfig (6%4) [Banjo, ElectricGuitarMuted]
  let t = 32 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = ionian
        , chords  = equally [maj, mi, dim]
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure

  let melodyConfig = Gen.GenConfig
        { Gen.key                = C
        , Gen.baseScale          = ionian
        , Gen.chords             = harmonicStructure
        , Gen.phraseDistribution = [(1, Gen.High), (0, Gen.Medium), (0, Gen.Low)]
        , Gen.octaveDistribution = [(3, 3), (5, 4), (2, 5)]
        }
  foreground <- Gen.runGenerator () (Gen.diatonicMelody melodyConfig)

  writeToMidiFile "out.midi" (((Rest $ 4 * wn) :+: foreground) :=: toMusicCore background)

rockOrganBlues :: IO ()
rockOrganBlues = do
  let ?midiConfig = MIDIConfig (6%4) [RockOrgan, AcousticGrandPiano]
  let t = 32 * wn

  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = ionian
        , chords  = equally [maj, mi, dim]
        }

  let harmonicStructure = foldr1 (:+:) $
        map (Note hn . uncurry instantiate)
          [ (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (A, d7)
          , (A, d7)
          , (A, d7)
          , (A, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (E, d7)
          , (B, d7)
          , (B, d7)
          , (A, d7)
          , (A, d7)
          , (E, d7)
          , (E, d7)
          , (B, d7)
          , (B, d7)
          ]
  let background = (flip (<#)) 3 <$> harmonicStructure

  let melodyConfig = Gen.GenConfig
        { Gen.key                = E
        , Gen.baseScale          = blues
        , Gen.chords             = 2##harmonicStructure
        , Gen.phraseDistribution = [(5, Gen.High), (5, Gen.Medium), (1, Gen.Low)]
        , Gen.octaveDistribution = [(3, 3), (5, 4), (2, 5)]
        }
  foreground <- Gen.runGenerator () (Gen.diatonicMelody melodyConfig)

  let melodyConfig' = Gen.GenConfig
        { Gen.key                = E
        , Gen.baseScale          = pentatonicMajor
        , Gen.chords             = 2##harmonicStructure
        , Gen.phraseDistribution = [(5, Gen.High), (5, Gen.Medium), (1, Gen.Low)]
        , Gen.octaveDistribution = [(2, 3), (5, 4), (2, 5)]
        }
  foreground' <- Gen.runGenerator () (Gen.diatonicMelody melodyConfig)

  writeToMidiFile "out.midi" ((foreground :+: foreground') :=: toMusicCore (4##background))

-- Hypnotic passage.
hypnotic :: Melody
hypnotic = 2%5 *~ cascades :+: (cascades ><)
  where
    cascades = rep id      (%> sn) 2 cascade
    cascade  = rep (~> M3) (%> en) 5 run
    run      = rep (~> P4) (%> tn) 5 (D#3 <| tn)
    rep :: (Melody -> Melody) -> (Melody -> Melody) -> Int -> Melody -> Melody
    rep _ _ 0 _ = (0~~)
    rep f g n m = m :=: g (rep f g (n - 1) (f m))

writeHypnotic :: IO ()
writeHypnotic = writeToMidiFile "hypnotic.mid" hypnotic
  where ?midiConfig = MIDIConfig 1 [RhodesPiano]

-- Byzantine dance for Harpsichord.
byzantineDance :: IO ()
byzantineDance = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = Fs
        , baseOct = Oct4
        , baseScale = byzantine
        , chords  = equally allChords
        }
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(1, Oct3), (20, Oct4), (15, Oct5), (1, Oct6)]
        }
  let ?midiConfig = MIDIConfig (7%4) [Harpsichord]
  (back, fore) <- integrate (8 * wn)

  writeToMidiFile "byzantine-h.mid" back
  writeToMidiFile "byzantine-m.mid" fore
  playDev 4 $ back :=: fore

-- Sonata in E Minor.
sonata :: IO ()
sonata = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = E
        , baseOct = Oct4
        , baseScale = minor
        , chords  = equally [mi, maj, dim]
        }
  let ?melodyConfig = defMelodyConfig
        { scales  = equally [ionian, harmonicMinor]
        , octaves = [(5, Oct4), (20, Oct5), (10, Oct6)]
        }
  let ?midiConfig = MIDIConfig (7%10) [AcousticGrandPiano, Flute]
  (back, fore) <- integrate (12 * wn)

  writeToMidiFile "sonata-h.mid" back
  writeToMidiFile "sonata-m.mid" fore
  playDev 4 $ back :=: fore

-- Romanian Elegy for Piano & Cello.
romanianElegy :: IO ()
romanianElegy = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = romanian
        , chords  = equally [mi, maj, aug, dim, m7, m7b5]
        }
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(20, Oct3), (15, Oct4), (10, Oct5)]
        }
  let ?midiConfig = MIDIConfig 1 [Harpsichord]
  (back, fore) <- integrate (12 * wn)

  writeToMidiFile "romanian-h.mid" back
  writeToMidiFile "romanian-m.mid" fore
  playDev 4 $ back :=: fore

-- Oriental Algebras for Metalophone, Sitar & Tablas.
orientalAlgebras :: IO ()
orientalAlgebras = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = A
        , baseOct = Oct3
        , baseScale = arabian
        , chords  = equally allChords
        }
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(20, Oct4), (15, Oct5), (5, Oct6)]
        , colorWeight = 0
        , approachWeight = 10
        }
  let ?midiConfig = MIDIConfig (6%5) [Harpsichord , Sitar]
  (back, fore) <- integrate (12 * wn)
  let ?tablaBeat = en
  rhythm <- dyn <$> runGrammar tabla (12 * wn) ()

  writeToMidiFile "oriental-h.mid" back
  writeToMidiFile "oriental-m.mid" fore
  writeToMidiFile "oriental-r.mid" rhythm
  playDev 4 $ back :=: fore
