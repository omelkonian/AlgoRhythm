module MIDI (writeToMidiFile) where

import Control.Arrow ((>>>))
import Sound.MIDI.File (T(..))
import Sound.MIDI.File.Save (toFile)
import Music

-- | Write `Music` to MIDI file.
writeToMidiFile :: (ToMusicCore a) => FilePath -> Music a -> IO ()
writeToMidiFile path = toMusicCore >>> musicToMidi >>> toFile path

-- | Convert `MusicCore` to `Sound.MIDI.File.T`.
musicToMidi :: MusicCore -> T
musicToMidi = undefined
