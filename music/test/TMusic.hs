{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PostfixOperators #-}
module TMusic where

import Control.Arrow                        ((>>>))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           ((@?=))
import Test.QuickCheck                      ((==>))

import GenSetup ()
import Music

musicTests :: Test
musicTests = testGroup "Music"
  [ testGroup "Instances"
      [ testCase "Functor" $
          const (D#5) <$> line [C#4<|qn, D#2<|wn] @?= line [D#5<|qn, D#5<|wn]
      , testCase "Foldable" $
          let f a = [a]
          in  foldMap f (line [C#4<|qn, D#2<|wn]) @?= [C#4, D#2]
      ]
  , testGroup "Transpose"
      [ testCase "a note" $
          C#4 <|hn ~> M3 @?= E#4<|hn
      , testCase "a chord" $
          let a  = chord $ C#4=|maj <|| 1
              a' = chord $ C#5=|maj <|| 1
          in  a ~> P8 @?= a'
      , testCase "a sequence of chords" $
          let a  = chord $ C#4=|maj7 <|| 1%8
              a' = chord $ Cs#4=|maj7 <|| 1%8
              b  = chord $ Ds#4=|aug <|| 3%4
              b' = chord $ E#4=|aug  <|| 3%4
          in  line [a, b, a] ~> Mi2 @?= line [a', b', a']
      , testCase "a scale" $
          let a  = line $ C#4+|minor <|| 1
              a' = line $ B#4+|minor <|| 1
          in  a ~> M7 @?= a'
      , testCase "a sequence of scales" $
          let a  = line $ C#4+|blues <|| 1%8
              a' = line $ Cs#4+|blues <|| 1%8
              b  = line $ Ds#4+|harmonicMinor <|| 3%4
              b' = line $ E#4+|harmonicMinor  <|| 3%4
          in  line [a, b, a] ~> Mi2 @?= line [a', b', a']
      , testProperty "identityUp" $ \(p :: Pitch) (d :: Duration) ->
          p<|d ~> P1 == p<|d
      , testProperty "identityDown" $ \(p :: Pitch) (d :: Duration) ->
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
  , testGroup "Invert"
      [ testCase "absolute pitches" $
          invert ([0, 10, -20] :: [AbsPitch]) @?= [0, -10, 20]
      , testCase "a melody" $
          let melody  = line [C#4<|hn, E#2<|wn, C#3<|en]
              melody' = line [C#4<|hn, Gs#5<|wn, C#5<|en]
          in  invert melody @?= melody'
      , testCase "a chord" $
          invert maj7 @?= [P1, Mi3, P5, Mi6]
      , testCase "a scale" $
          mode vi ionian @?= minor
      ]
  , testGroup "Retro"
      [ testCase "a melody" $
          (line [C#4<|hn, (wn.-), Gs#4<|en] ><) @?=
            line [Gs#4<|en, (wn.-), C#4<|hn]
      ]
  , testGroup "TimeScaling"
      []
  , testGroup "Other"
      [ testCase "toList" $
          musicToList (C#4<|hn :+: Rest wn :+: C#5<|qn) @?=
            [(Just $ C#4, hn), (Nothing, wn), (Just $ C#5, qn)]
      , testCase "fromList" $
          listToMusic [(Just $ C#4, hn), (Nothing, wn), (Just $ C#5, qn)] @?=
            (C#4<|hn :+: Rest wn :+: C#5<|qn)
      , testCase "normalize" $
          let m = (wn.-) :: Melody
          in  normalize ((m :+: m) :+: m) @?= m :+: m :+: m
      ]
  ]
