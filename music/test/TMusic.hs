{-# LANGUAGE ScopedTypeVariables #-}
module TMusic where

import           Control.Arrow                        ((>>>))
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck ((==>))

import           Music
import           GenSetup()

musicTests = testGroup "Music" [transpTests]

transpTests = testGroup "Tranpose"
  [ testCase "one note" $
      C#4 <|hn ~> M3 @?= E#4<|hn
  , testCase "one chord" $
      let a  = chord $ C#4=|maj <|| 1
          a' = chord $ C#5=|maj <|| 1
      in  a ~> P8 @?= a'
  , testCase "sequence of chords" $
      let a  = chord $ C#4=|maj7 <|| 1%8
          a' = chord $ Cs#4=|maj7 <|| 1%8
          b  = chord $ Ds#4=|aug <|| 3%4
          b' = chord $ E#4=|aug  <|| 3%4
      in  line [a, b, a] ~> Mi2 @?= line [a', b', a']
  , testCase "one scale" $
      let a  = line $ C#4+|minor <|| 1
          a' = line $ B#4+|minor <|| 1
      in  a ~> M7 @?= a'
  , testCase "sequence of scales" $
      let a  = line $ C#4+|blues <|| 1%8
          a' = line $ Cs#4+|blues <|| 1%8
          b  = line $ Ds#4+|harmonicMinor <|| 3%4
          b' = line $ E#4+|harmonicMinor  <|| 3%4
      in  line [a, b, a] ~> Mi2 @?= line [a', b', a']
  , testProperty "identityUp" $ \(p :: Pitch) (d :: Duration) ->
      p<|d ~> P1 == p<|d
  , testProperty "identityUp" $ \(p :: Pitch) (d :: Duration) ->
      p<|d <~ P1 == p<|d
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
