{-# LANGUAGE GADTs, BangPatterns, ImplicitParams #-}

module Generate.Chaos where

import Music
import Utils.Vec
import Generate.Generate
import Export
import Control.Monad.State hiding (state)

data Mapping n = Mapping { pcSel  :: Selector (ChaosState n) PitchClass
                         , octSel :: Selector (ChaosState n) Octave
                         , durSel :: Selector (ChaosState n) Duration
                         , itvSel :: Selector (ChaosState n) Interval
                         , dynSel :: Selector (ChaosState n) Dynamic
                         , artSel :: Selector (ChaosState n) Articulation
                         }

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
  (_, s') <- runStateT genNextIteration s
  return (snd (head as), s')

chaosEntry :: (Enum a, Bounded a) => ChaosState n -> Selector (ChaosState n) a -> Entry (ChaosState n) a
chaosEntry _ sel = Entry { values      = zip (repeat 1) [minBound ..]
                     , constraints = []
                     , selector    = sel
                     }

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
runGenerator :: ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) a -> IO a
runGenerator s m g = runGenerator' (chaosState s m) g

-- runGenerator :: s -> MusicGenerator s a -> IO a
-- runGenerator = runGenerator' . quickCheckState

clean :: ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) a -> MusicGenerator (ChaosState n) a
clean s m = modified (const $ chaosState s m)

playGen :: ToMusicCore a => ChaosState n -> Mapping n -> MusicGenerator (ChaosState n) (Music a) -> IO ()
playGen s m gen = do
  music <- runGenerator s m gen
  let ?midiConfig = defaultMIDIConfig
  playDev 0 music

-- | Builds a ChaosState from two Vectors of the same length. This constraint
--   is imposed since the number of variables should be equal to the number
--   of update functions.
buildChaos :: Vec n Double                   -- ^ Initial variable values
           -> Vec n (Vec n Double -> Double) -- ^ Functions that calculate next variable values
           -> ChaosState n
buildChaos vs fs = ChaosState { variables = vs , updateFunctions = fs }

data ChaosState n =
  ChaosState { variables       :: Vec n Double
             , updateFunctions :: Vec n (Vec n Double -> Double)
             }

type ChaosGenerator n = StateT (ChaosState n) IO

genNextIteration :: ChaosGenerator n [Double]
genNextIteration = do
    s <- get
    let vs = variables s
    let fs = updateFunctions s
    let newVs = fmap (\f -> f vs) fs
    put (s { variables = newVs })
    return $ list newVs
