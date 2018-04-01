{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Export
import Grammar
import Music
import Dynamics

main :: IO ()
main = do
  let ?midiConfig = MIDIConfig (6%5) [AcousticGrandPiano]
  let t = 4 * wn


  let ?harmonyConfig = HarmonyConfig
        { basePc  = A
        , baseOct = Oct4
        , baseScale = minor
        , chords  =
            [ (10, maj), (10, mi)
            -- , (8, d7), (8, maj6), (8, m6)
            -- , (7, maj7), (7, m7)
            , (6, dim), (6, aug)
            -- , (6, sus4), (6, d7sus4)
            -- , (5, dim7), (5, m7b5)
            -- , (4, maj9), (4, m9), (4, d9)
            -- , (1, d7b5), (1, d7s5), (1, d7b9), (1, d7s9), (1, d7b5b9)
            ]
        }
  harmonicStructure <- runGrammar uuHarmony t ?harmonyConfig
  background <- voiceLead harmonicStructure

  let ?melodyConfig = MelodyConfig
        { scales  =
            [ (10, ionian)
            -- , (10, dorian), (10, phrygian), (10, lydian)
            -- , (10, mixolydian), (10, aeolian), (10, pentatonicMajor)
            -- , (10, locrian), (10, minor), (10, harmonicMinor)
            -- , (10, melodicMinor), (10, pentatonicMinor), (10, blues)
            -- , (5, bebopDominant), (5, bebopDorian), (5, bebopMajor)
            -- , (5, bebopMelodicMinor), (5, bebopHarmonicMinor)
            -- , (5, altered), (5, wholeTone), (5, halfDiminished), (5, flamenco)
            ]
        , octaves = [(1, Oct3), (20, Oct4), (15, Oct5), (1, Oct6)]
        }
<<<<<<< HEAD
  let ?midiConfig = MIDIConfig (4%4) [BrassSection, AcousticGrandPiano]

  cp <- final (4 * wn)
  -- writeToMidiFile "cp.midi" midiConfig cp
  -- putStrLn "Wrote to MIDI."
  -- writeToLilypondFile "cp.ly" cp
  -- putStrLn "Wrote to Lilypond"
  writeToLilypondFile "out.ly" cp
  writeToMidiFile "out.midi" cp
  putStrLn "Playback finished"
  -- tab <- tablaTest
  -- writeToMidiFile "tablas.midi" defaultMIDIConfig tab
  -- putStrLn "Wrote to MIDI."
=======
  melodicStructure <- runGrammar melody t ()
  foreground <- mkSolo harmonicStructure melodicStructure
>>>>>>> 5caaf9d4d624c4ae62feb76cbd0c4e736fdb13f2

  playDev 4 $
    5000 ##
      addDynamics (toMusicCore background :=: toMusicCore foreground)

  -- let ?tablaBeat = tn
  -- rhythm <- runGrammar tabla t ()
  -- writeToMidiFile "rhythm.midi" rhythm
  -- putStrLn "Wrote rhythm to MIDI."
