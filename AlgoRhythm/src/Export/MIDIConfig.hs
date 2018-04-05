-- | Can be used by `Export.MIDI` to specify instruments, tempo and other
--   global music configurations in the exported MIDI file.
module Export.MIDIConfig (
    E.InstrumentName (..)
  , MIDIConfig (..)
  , defaultMIDIConfig
) where

import qualified Euterpea as E

-- | The tempo of the music (1 would be a standard tempo.)
type Tempo = Rational
-- | Stores metadata that will be added to the Midi file on export.
data MIDIConfig = MIDIConfig { tempo      :: Tempo
                             , instruments :: [E.InstrumentName]
                             }

-- | Standard `MIDIConfig` with a `Tempo` of 1 and an `AcousticGrandPiano` as
--   instrument.
defaultMIDIConfig :: MIDIConfig
defaultMIDIConfig = MIDIConfig { tempo      = 1
                               , instruments = [E.AcousticGrandPiano]
                               }
