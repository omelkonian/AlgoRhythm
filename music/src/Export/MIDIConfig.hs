-- | Can be used by the MIDI module to specify instruments, tempo and other
--   global music configurations in the exported MIDI file.
module Export.MIDIConfig (
    E.InstrumentName (..)
  , MIDIConfig (..)
  , defaultMIDIConfig
  , controlsE
) where

import qualified Euterpea as E

type Tempo = Rational
data MIDIConfig = MIDIConfig { tempo      :: Tempo
                             , instrument :: E.InstrumentName
                             }

defaultMIDIConfig :: MIDIConfig
defaultMIDIConfig = MIDIConfig { tempo      = 1
                               , instrument = E.AcousticGrandPiano }

-- | Converts all values stored in the given MIDIConfig to Euterpea Control
--   elements and returns the results in a list.
controlsE :: MIDIConfig -> [E.Control]
controlsE c = [ E.Instrument (instrument c)
              , E.Tempo (tempo c)
              ]
