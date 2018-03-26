{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# language DataKinds #-}
module TVec where

import Utils.Vec
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           ((@?=))

len :: Vec n a -> Integer
len v = foldr (\_ -> (1+)) 0 v

addElem :: a -> Vec n a -> Vec (S n) a
addElem a v = a :. v

sameElems :: Eq a => Vec n a -> [a] -> Bool
sameElems  Nil    []     = True
sameElems (y:.ys) (x:xs) = x == y && sameElems ys xs
sameElems  _       _     = False

-- Since it's pretty much impossible to generate arbitrary Vecs that have
-- their length encoded in their type, only some hardcoded Vecs are tested.
vecTests :: Test
vecTests = testGroup "Vec"
  [ testCase "0-elem vec length" $
      len v0 @?= 0
  , testCase "0-elem vec list length" $
      length (list v0) @?= 0
  , testCase "0-elem vec list same elems" $
      sameElems v0 [] @?= True
  , testCase "0-elem vec addElem" $
      (addElem 1 v0) @?= v1

  , testCase "1-elem vec length" $
      len v1 @?= 1
  , testCase "1-elem vec list length" $
      length (list v1) @?= 1
  , testCase "1-elem vec list same elems" $
      sameElems v1 [1] @?= True
  , testCase "1-elem vec addElem" $
      addElem 2 v1 @?= v2

  , testCase "2-elem vec length" $
      len v2 @?= 2
  , testCase "2-elem vec list length" $
      length (list v2) @?= 2
  , testCase "2-elem vec list same elems" $
      sameElems v2 [2,1] @?= True
  , testCase "2-elem vec addElem" $
      addElem 3 v2 @?= v3
  ]
  where v0 = Nil :: Vec D0 Int
        v1 = 1 :. Nil
        v2 = 2 :. 1 :. Nil
        v3 = 3 :. 2 :. 1 :. Nil
