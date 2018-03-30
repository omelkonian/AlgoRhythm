-- | Can be used by the MIDI module to specify instruments, tempo and other
--   global music configurations in the exported MIDI file.
module Export.MIDIConfig (
    E.InstrumentName (..)
  , MIDIConfig (..)
  , defaultMIDIConfig
) where

import qualified Euterpea as E

type Tempo = Rational
data MIDIConfig = MIDIConfig { tempo      :: Tempo
                             , instruments :: [E.InstrumentName]
                             }

defaultMIDIConfig :: MIDIConfig
defaultMIDIConfig = MIDIConfig { tempo      = 1
                               , instruments = [E.AcousticGrandPiano]
                               }
