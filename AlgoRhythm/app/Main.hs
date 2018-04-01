{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Dynamics
import Export
import Grammar
import Music

main :: IO ()
main = do
  let ?midiConfig = MIDIConfig (6%5) [AcousticGuitarSteel]
  let t = 4 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = japanese
        , chords  = equally allChords
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure

  -- Melody.
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(1, Oct3), (20, Oct4), (15, Oct5), (1, Oct6)]
        }
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure

  playDev 4 $ 2 ## dyn (toMusicCore background :=: toMusicCore foreground)

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

-- Byzantine dance in F#.
byzantineDance :: IO ()
byzantineDance = do
  let ?midiConfig = MIDIConfig (7%4) [Harpsichord]
  let t = 8 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = Fs
        , baseOct = Oct4
        , baseScale = byzantine
        , chords  = equally allChords
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure
  writeToMidiFile "byzantine-h.mid" (dyn background)

  -- Melody.
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(1, Oct3), (20, Oct4), (15, Oct5), (1, Oct6)]
        }
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure
  writeToMidiFile "byzantine-m.mid" (dyn foreground)

  playDev 4 $ dyn $ toMusicCore background :=: toMusicCore foreground

-- Sonata in E minor.
sonata :: IO ()
sonata = do
  let ?midiConfig = MIDIConfig (7%10) [AcousticGrandPiano, Flute]
  let t = 12 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = E
        , baseOct = Oct4
        , baseScale = minor
        , chords  = equally [mi, maj, dim]
        }
  harmonicStructure <- runGrammar tonalHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure
  writeToMidiFile "sonata-h.mid" (dyn background)

  -- Melody.
  let ?melodyConfig = defMelodyConfig
        { scales  = equally [ionian, harmonicMinor]
        , octaves = [(5, Oct4), (20, Oct5), (10, Oct6)]
        }
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure
  writeToMidiFile "sonata-m.mid" (dyn foreground)

  playDev 4 $ dyn $ toMusicCore background :=: toMusicCore foreground

-- Romanian Elegy in Ds minor.
romanianElegy :: IO ()
romanianElegy = do
  let ?midiConfig = MIDIConfig 1 [Harpsichord]
  let t = 12 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = C
        , baseOct = Oct4
        , baseScale = romanian
        , chords  = equally [mi, maj, aug, dim, m7, m7b5]
        }
  harmonicStructure <- runGrammar tonalHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure
  writeToMidiFile "romanian-h.mid" (dyn background)

  -- Melody.
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(20, Oct3), (15, Oct4), (10, Oct5)]
        }
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure
  writeToMidiFile "romanian-m.mid" (dyn foreground)

  playDev 4 $ dyn $ toMusicCore background :=: toMusicCore foreground

-- Magnum opus.
magnumOpus :: IO ()
magnumOpus = do
  let ?midiConfig = MIDIConfig (6%5) [Harpsichord , Sitar]
  let t = 12 * wn

  -- Harmony.
  let ?harmonyConfig = HarmonyConfig
        { basePc  = A
        , baseOct = Oct3
        , baseScale = arabian
        , chords  = equally allChords
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure
  writeToMidiFile "magnum-h.mid" (dyn background)

  -- Melody.
  let ?melodyConfig = defMelodyConfig
        { scales  = equally allScales
        , octaves = [(20, Oct4), (15, Oct5), (5, Oct6)]
        , colorWeight = 0
        , approachWeight = 10
        }
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure
  writeToMidiFile "magnum-m.mid" (dyn foreground)

  -- Rhythm.
  let ?tablaBeat = en
  rhythm <- runGrammar tabla t ()
  writeToMidiFile "magnum-r.mid" (dyn rhythm)

  playDev 4 $ dyn $ toMusicCore background :=: toMusicCore foreground
