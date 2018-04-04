{-# language GADTs #-}

module TChaos where

import           System.Directory               (doesFileExist)
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.Framework                 (testGroup, buildTestBracketed)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.API   (buildTest)
import           Test.HUnit
import Generate.Applications.ChaosPitches
import Control.Monad.Trans.State
import qualified Generate as Gen
import Utils.Vec

-- | Tests if the Chaos function correctly updates every iteration.
chaosTests = testGroup "Chaos"
  [ testCase "chaos1" $ do
      let mapping = Gen.defaultMapping {Gen.pcSel=Gen.chaos1Selector, Gen.octSel=Gen.chaos1Selector}
      (music,genState) <- runStateT Gen.bSolo (Gen.chaosState Gen.chaos1 mapping)
      let chaosState = Gen.state genState
      let ds = Gen.variables   chaosState
      case Gen.updateFunctions chaosState of
        (f :. Nil) -> do
          let expectedVs = [ -1.0
                           , -0.99
                           , -0.950399
                           , -0.7974839358099899
                           , -0.2656014494712342
                           ,  0.859617181377171
                           , -0.4704939800524761
                           ,  0.5594844753214167
                           ,  0.3770844725298951
                           ,  0.7170365281479332
                           ]
          let actualVs = reverse $ snd $ foldr (\_ (d',vs) -> (f d' :. Nil, f d' : vs)) (ds, []) [1..10]
          actualVs @?= expectedVs
  ]
