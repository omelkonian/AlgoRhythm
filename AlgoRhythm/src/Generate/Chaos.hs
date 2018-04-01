{-# LANGUAGE GADTs, BangPatterns, ImplicitParams #-}

module Generate.Chaos where

import Music
import Utils.Vec
import Generate.Generate
import Export
import Control.Monad (void)
import Control.Monad.State hiding (state)
import System.IO.Unsafe

chaosSelector :: Selector (ChaosState n) a
chaosSelector s as = do
  (ds, s') <- runStateT genNextIteration s
  let d = head ds
  let a = as !! (round (d * 10e100) `mod` length as)
  let !_ = unsafePerformIO $ print $ show ds
  return (snd a, s')

chaosEntry :: (Enum a, Bounded a) => ChaosState n -> Entry (ChaosState n) a
chaosEntry _ = Entry { values      = zip (repeat 1) [minBound ..]
                          , constraints = []
                          , selector    = chaosSelector
                          }

chaosState :: ChaosState n -> GenState (ChaosState n)
chaosState st = GenState { state = st
                         , pc  = chaosEntry st
                         , oct = chaosEntry st
                         , dur = Entry { values =
                                           zip (repeat 1) [1%1,1%2,1%4,1%8,1%16]
                                       , constraints = []
                                       , selector    = chaosSelector
                                       }
                         , itv = chaosEntry st
                         , dyn = chaosEntry st
                         , art = chaosEntry st
                         }

-- | Runs a generator on the chaos state.
runGenerator :: ChaosState n -> MusicGenerator (ChaosState n) a -> IO a
runGenerator = runGenerator' . chaosState

-- runGenerator :: s -> MusicGenerator s a -> IO a
-- runGenerator = runGenerator' . quickCheckState

clean :: ChaosState n -> MusicGenerator (ChaosState n) a -> MusicGenerator (ChaosState n) a
clean s = modified (const $ chaosState s)

playGen :: ToMusicCore a => ChaosState n -> MusicGenerator (ChaosState n) (Music a) -> IO ()
playGen s music = do
  m <- runGenerator s music
  let ?midiConfig = defaultMIDIConfig
  playDev 4 m

-- | Builds a ChaosState from two Vectors of the same length. This constraint
--   is imposed since the number of variables should be equal to the number
--   of update functions.
buildChaos :: Vec n Double                   -- ^ Initial variable values
           -> Vec n (Vec n Double -> Double) -- ^ Functions that calculate next variable values
           -> ChaosState n
buildChaos vs fs = ChaosState { variables = vs , updateFunctions = fs }

main' :: IO ()
main' = void (runStateT genNextIteration chaos1)

data ChaosState n =
  ChaosState { variables       :: Vec n Double
             , updateFunctions :: Vec n (Vec n Double -> Double)
             }

chaos1 :: ChaosState D2
chaos1 = buildChaos (0.2 :. 0.2 :. Nil) (f1 :. f2 :. Nil)
  where f1 :: (Vec D2 Double -> Double)
        f1 vs@(x:._:.Nil) = f2 vs - 0.5 * x ** 2
        f2 :: (Vec D2 Double -> Double)
        f2    (x:._:.Nil) = 0.5 * x

type ChaosGenerator n = StateT (ChaosState n) IO

genNextIteration :: ChaosGenerator n [Double]
genNextIteration = do
    s <- get
    let vs = variables s
    let fs = updateFunctions s
    let newVs = fmap (\f -> f vs) fs
    put (s { variables = newVs })
    return $ list newVs
