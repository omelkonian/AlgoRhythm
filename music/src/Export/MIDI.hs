module Export.MIDI (
    module Export.MIDIConfig
  , writeToMidiFile
  , play
  , playDev
) where

import           Codec.Midi
import           Control.Arrow ((>>>))
import           Data.Ratio    ((%))
import           Export.MIDIConfig
import qualified Euterpea as E
import           Music

-- | Write `Music` to MIDI file.
writeToMidiFile :: (ToMusicCore a) => FilePath -> MIDIConfig -> Music a -> IO ()
writeToMidiFile path c = toMusicCore >>> musicToMidi c >>> E.exportMidiFile path

-- | Plays `Music` to the given MIDI output device (using Euterpea under the
--   hood).
playDev :: (ToMusicCore a) => Int -> MIDIConfig -> Music a -> IO ()
playDev devId c = toMusicCore >>> musicToE c >>> E.playDev devId

-- | Plays `Music` to the standard MIDI output device.
play :: (ToMusicCore a) => MIDIConfig -> Music a -> IO ()
play c = toMusicCore >>> musicToE c >>> E.play

-- | Converts `MusicCore` to `Codec.Midi.Midi`. Note that this is done using
--   Euterpea's toMidi function, which does not return a Euterpea defined
--   Midi type, but rather a Midi type from the HCodecs library.
musicToMidi :: MIDIConfig -> MusicCore -> Midi
musicToMidi c m = E.toMidi $ E.perform $ musicToE c m

-- | Converts MusicCore to Euterpea Music1 using a MIDIConfig.
musicToE :: MIDIConfig -> MusicCore -> E.Music1
musicToE c m = foldr E.Modify (musicToE' m) (controlsE c)

-- | Converts `MusicCore` to Euterpea Music1
musicToE' :: MusicCore -> E.Music1
musicToE' (m :+: m')            = (E.:+:) (musicToE' m) (musicToE' m')
musicToE' (m :=: m')            = (E.:=:) (musicToE' m) (musicToE' m')
musicToE' (Rest dur)            = E.rest dur
musicToE' (Note dur (p, attrs)) = noteToE dur (p, attrs)

-- | Converts MusicCore Note to a Euterpea Music1 Note.
noteToE :: Duration -> FullPitch -> E.Music1
noteToE dur (p, attrs) = do
  -- Initially create a note with pitch and duration, but no extra attributes.
  let noteE = E.note dur (pitchToE p, [])
  -- Add the attributes one by one.
  foldr (flip addAttrToE) noteE attrs

-- | Converts `Pitch` to a Euterpea Pitch.
pitchToE :: Pitch -> E.Pitch
pitchToE (pc, oct) = (pitchClassToE pc, fromEnum oct)

-- | Converts `PitchClass` to a Euterpea PitchClass.
pitchClassToE :: PitchClass -> E.PitchClass
pitchClassToE p = case p of
  C  -> E.C
  Cs -> E.Cs
  D  -> E.D
  Ds -> E.Ds
  E  -> E.E
  F  -> E.F
  Fs -> E.Fs
  G  -> E.G
  Gs -> E.Gs
  A  -> E.A
  As -> E.As
  B  -> E.B

addAttrToE :: E.Music1 -> PitchAttribute -> E.Music1
addAttrToE n a = E.Modify (E.Phrase [attrToE a]) n

-- | Converts a PitchAttribute to its Euterpea representation.
attrToE :: PitchAttribute -> E.PhraseAttribute
attrToE (Dynamic d)      = E.Dyn $ dynamicsToE d
attrToE (Articulation a) = E.Art $ articulationToE a

-- | Converts Dynamics to Euterpea Dynamic.
dynamicsToE :: Dynamic -> E.Dynamic
dynamicsToE d = E.StdLoudness dE
  where -- There are 11 Dynamics in the Music DSL and only 9 in the Euterpea
        -- DSL. Hence the code below. The magic 8 represents the maximum
        -- fromEnum value one can get from an E.Dynamic value. However, Euterpea
        -- has not derived Bounded for E.Dynamic, so maxBound::E.Dynamic
        -- couldn't be used here.
        dE = toEnum (min 8 (max 0 (fromEnum d) - 1))

-- | Converts Articulation to Euterpea Articulation.
articulationToE :: Articulation -> E.Articulation
articulationToE Staccato      = E.Staccato (1%4)
articulationToE Staccatissimo = E.Staccato (1%8)
articulationToE Marcato       = E.Marcato
articulationToE Tenuto        = E.Tenuto
