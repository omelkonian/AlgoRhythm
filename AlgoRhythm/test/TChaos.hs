{-# LANGUAGE GADTs #-}

module TChaos where

import           Control.Monad.Trans.State
import qualified Generate                       as Gen
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test)
import           Utils.Vec

-- | Tests if the Chaos function correctly updates every iteration.
chaosTests :: Test
chaosTests = testGroup "Chaos"
  [ testCase "chaos1" $ do
      let mapping = Gen.defaultMapping {Gen.pcSel=Gen.chaos1Selector, Gen.octSel=Gen.chaos1Selector}
      (_, genState) <- runStateT Gen.bSolo (Gen.chaosState Gen.chaos1 mapping)
      let chaosState = Gen.state genState
      let ds = Gen.variables   chaosState
      case Gen.updateFunctions chaosState of
        (f :. Nil) -> do
          let expectedVs = [ -1.0
                           , -0.9521
                           , -0.7695677377609997
                           , -0.15610097331134187
                           ,  0.9524321761768165
                           , -0.7708027147284231
                           , -0.15981449614634702
                           ,  0.9501420518882291
                           , -0.7622971584238392
                           , -0.1343593712063227
                           ]
          let actualVs = reverse
                       $ snd
                       $ foldr (\_ (d',vs) -> (f d' :. Nil, f d' : vs)) (ds, []) [(1 :: Int)..10]
          actualVs @?= expectedVs
  ]
