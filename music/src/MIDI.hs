module MIDI (writeToMidiFile, play, playDev) where

import Control.Arrow ((>>>))
import qualified Euterpea as E
import Codec.Midi
import Music

-- | Write `Music` to MIDI file.
writeToMidiFile :: (ToMusicCore a) => FilePath -> Music a -> IO ()
writeToMidiFile path = toMusicCore >>> musicToMidi >>> E.exportMidiFile path

-- | Plays `Music` to the given MIDI output device (using Euterpea under the
--   hood).
playDev :: (ToMusicCore a) => Int -> Music a -> IO ()
playDev i = toMusicCore >>> musicToE >>> E.playDev i

-- | Plays `Music` to the standard MIDI output device.
play :: (ToMusicCore a) => Music a -> IO ()
play = toMusicCore >>> musicToE >>> E.play

-- | Converts `MusicCore` to `Codec.Midi.Midi`. Note that this is done using
--   Euterpea's toMidi function, which does not return a Euterpea defined
--   Midi type, but rather a Midi type from the HCodecs library.
musicToMidi :: MusicCore -> Midi
musicToMidi m = E.toMidi (E.perform (musicToE m))

-- | Converts `MusicCore` to Euterpea Music1
musicToE :: MusicCore -> E.Music1
musicToE (m :+: m')           = (E.:+:) (musicToE m) (musicToE m')
musicToE (m :=: m')           = (E.:=:) (musicToE m) (musicToE m')
musicToE (Rest dur)           = E.rest dur
musicToE (Note dur fullPitch) = E.note dur (fullPitchToE fullPitch)

-- | Converts `FullPitch` to a Euterpea Note1
fullPitchToE :: FullPitch -> E.Note1
fullPitchToE (p, attrs) = (pitchToE p, pitchAttrsToE attrs)

-- | Converts `Pitch` to a Euterpea Pitch
pitchToE :: Pitch -> E.Pitch
pitchToE (pClass, oct) = (toEnum (fromEnum pClass), fromEnum oct)

pitchAttrsToE :: [PitchAttribute] -> [E.NoteAttribute]
pitchAttrsToE = map pitchAttrToE

pitchAttrToE :: PitchAttribute -> E.NoteAttribute
pitchAttrToE (Dynamics d)     = dynamicsToE     d
pitchAttrToE (Articulation a) = articulationToE a

dynamicsToE :: Dynamics -> E.NoteAttribute
dynamicsToE = E.Dynamics . show

articulationToE :: Articulation -> E.NoteAttribute
articulationToE a = undefined
