{-# LANGUAGE GADTs #-}

module Music.Chaos where

import Control.Monad.State
import Control.Monad (void)
import Music.Vec
import Data.TypeLevel.Num hiding ((-), (+), (*), (/))

generate :: ChaosState n -> [(Double, a)] -> IO (a, ChaosState n)
generate s as = do
  (ds, s') <- runStateT genNextIteration s
  return (snd $ head as, s')


-- | Builds a ChaosState from two Vectors of the same length. This constraint
--   is imposed since the number of variables should be equal to the number
--   of update functions.
buildChaos :: Vec Double n                   -- ^ Initial variable values
           -> Vec (Vec Double n -> Double) n -- ^ Functions that calculate next variable values
           -> ChaosState n
buildChaos vs fs = ChaosState { variables = vs , updateFunctions = fs }

main :: IO ()
main = void (runStateT genNextIteration chaos1)

data ChaosState n =
  ChaosState { variables       :: Vec Double n
             , updateFunctions :: Vec (Vec Double n -> Double) n
             }

chaos1 :: ChaosState D2
chaos1 = buildChaos (0.2 :. 0.2 :. Nil) (f1 :. f2 :. Nil)
  where f1 :: (Vec Double D2 -> Double)
        f1 vs@(x:._:.Nil) = f2 vs - 0.5 * x ** 2
        f2 :: (Vec Double D2 -> Double)
        f2    (x:._:.Nil) = 0.5 * x

type ChaosGenerator n = StateT (ChaosState n) IO

genNextIteration :: ChaosGenerator n [Double]
genNextIteration = do
    s <- get
    let vs = variables s
    let fs = updateFunctions s
    let newVs = map' (\f -> f vs) fs
    put (s { variables = newVs })
    return $ toList newVs
