{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

-- | A `MusicGenerator` that uses Chaos functions.

module Generate.Chaos where

import Music
import Utils.Vec
import Generate.Generate
import Export
import Control.Monad.State hiding (state)

-- | Selectors for all `Generate.Generate.GenState` elements.
data Mapping n = Mapping { pcSel  :: Selector (ChaosState n) PitchClass
                         , octSel :: Selector (ChaosState n) Octave
                         , durSel :: Selector (ChaosState n) Duration
                         , itvSel :: Selector (ChaosState n) Interval
                         , dynSel :: Selector (ChaosState n) Dynamic
                         , artSel :: Selector (ChaosState n) Articulation
                         }

-- | Default `Mapping` that just grabs the first element from the list of
--   possible values.
defaultMapping :: Mapping n
defaultMapping = Mapping  { pcSel  = defaultChaosSelector
                          , octSel = defaultChaosSelector
                          , durSel = defaultChaosSelector
                          , itvSel = defaultChaosSelector
                          , dynSel = defaultChaosSelector
                          , artSel = defaultChaosSelector
                          }

-- | Default Chaos selector, (just grabs the first element from the list).
defaultChaosSelector :: Selector (ChaosState n) a
defaultChaosSelector s as = do
  return (snd (head as), s)

-- | Generates an `Entry` based on a `ChaosState` and `Selector`.
chaosEntry :: (Enum a, Bounded a) => ChaosState n -> Selector (ChaosState n) a -> Entry (ChaosState n) a
chaosEntry _ sel = Entry { values      = zip (repeat 1) [minBound ..]
                     , constraints = []
                     , selector    = sel
                     }

-- | Builds a `GenState` with a `ChaosState` based on a `ChaosState` and `Mapping`
chaosState :: ChaosState n -> Mapping n -> GenState (ChaosState n)
chaosState st m = GenState { state = st
                         , pc  = chaosEntry st (pcSel m)
                         , oct = chaosEntry st (octSel m)
                         , dur = Entry { values =
                                           zip (repeat 1) [1%1,1%2,1%4,1%8,1%16]
                                       , constraints = []
                                       , selector    = (durSel m)
                                       }
                         , itv = chaosEntry st (itvSel m)
                         , dyn = chaosEntry st (dynSel m)
                         , art = chaosEntry st (artSel m)
                         }

-- | Runs a generator on the chaos state.
runChaosGenerator :: ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) a -> IO a
runChaosGenerator s m g = runGenerator' (chaosState s m) g

-- | Cleans the `MusicGenerator`
cleanChaos :: ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) a -> MusicGenerator (ChaosState n) a
cleanChaos s m = modified (const $ chaosState s m)

-- | Generates music and plays it using Midi on device 0.
playChaosGen :: ToMusicCore a => ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) (Music a) -> IO ()
playChaosGen s m gen = do
  music <- runChaosGenerator s m gen
  let ?midiConfig = defaultMIDIConfig
  playDev 0 music

-- | Builds a ChaosState from two Vectors of the same length. This constraint
--   is imposed since the number of variables should be equal to the number
--   of update functions.
buildChaos :: Vec n Double                   -- ^ Initial variable values
           -> Vec n (Vec n Double -> Double) -- ^ Functions that calculate next variable values
           -> ChaosState n
buildChaos vs fs = ChaosState { variables=vs , updateFunctions=fs}

-- | The default `ChaosState` that is used for Chaotic generation.
data ChaosState n =
  ChaosState { variables       :: Vec n Double
             , updateFunctions :: Vec n (Vec n Double -> Double)
             }

-- | The `ChaosState wrapped in a `StateT` monad.`
type ChaosGenerator n = StateT (ChaosState n) IO

-- | Calculates the next iteration of values for the `ChaosState`.
genNextIteration :: ChaosGenerator n [Double]
genNextIteration = do
    s <- get
    let vs = variables s
    let fs = updateFunctions s
    let newVs = fmap (\f -> f vs) fs
    put (s { variables = newVs })
    return $ list newVs
