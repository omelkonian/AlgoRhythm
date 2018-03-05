{-# LANGUAGE ScopedTypeVariables #-}
module TMusic where

import           Control.Arrow                        ((>>>))
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck ((==>))

import           Music
import           GenSetup

import Debug.Trace

musicTests = testGroup "Music" [transpTests]

transpTests = testGroup "Tranpose"
  [ testCase "one note" $
      C<@4 <| 5%4 ~> M3 @?= E<@4 <| 5%4
  , testCase "one chord" $
      let a  = chord [p<@4<|4%4 | p <- [C, E, G]]
          a' = chord [p<@4<|4%4 | p <- [Cs, F, Gs]]
      in  a ~> Min2 @?= a'
  , testCase "sequence of chords" $
      let a  = chord [p<@4<|4%4 | p <- [C, E, G]]
          a' = chord [p<@4<|4%4 | p <- [Cs, F, Gs]]
          b  = chord [p<@4<|4%4 | p <- [D, Fs, A]]
          b' = chord [p<@4<|4%4 | p <- [Ds, G, As]]
      in  line [a, b, a] ~> Min2 @?= line [a', b', a']
  , testProperty "identityUp" $ \(p :: Pitch) (d :: Duration) ->
      (p<|d ~> P1) == (p<|d)
  , testProperty "identityUp" $ \(p :: Pitch) (d :: Duration) ->
      (p<|d <~ P1) == (p<|d)
  , testProperty "commutativeUp" $
      \(p :: Pitch) (m :: Interval) (n :: Interval) (d :: Duration) ->
        ((~> m) >>> (~> n)) (p<|d) == ((~> n) >>> (~> m)) (p<|d)
  , testProperty "commutativeDown" $
      \(p :: Pitch) (m :: Interval) (n :: Interval) (d :: Duration) ->
        ((<~ m) >>> (<~ n)) (p<|d) == ((<~ n) >>> (<~ m)) (p<|d)
  , testProperty "erasure" $
      \(p :: Pitch) (m :: Interval) (d :: Duration) ->
        fromEnum p + fromEnum m <= fromEnum (maxBound :: Pitch) ==>
        ((~> m) >>> (<~ m)) (p<|d) == (p<|d)
  ]
