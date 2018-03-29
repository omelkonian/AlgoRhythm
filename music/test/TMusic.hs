{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TMusic where

import Control.Arrow                        ((>>>))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           ((@?=))
import Test.QuickCheck                      ((==>))

import GenSetup
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
      [ testCase "a pitch class" $
          C ~> M3 @?= E
      , testCase "a pitch" $
          C#4 ~> M7 @?= B#4
      , testCase "a note" $
          C#4 <|hn ~> M3 @?= E#4<|hn
      , testCase "a chord" $
          let a  = chord $ C#4=|maj <|| def
              a' = chord $ C#5=|maj <|| def
          in  a ~> P8 @?= a'
      , testCase "a sequence of chords" $
          let a  = chord $ C#4=|maj7 <|| 1%8
              a' = chord $ Cs#4=|maj7 <|| 1%8
              b  = chord $ Ds#4=|aug <|| def
              b' = chord $ E#4=|aug  <|| def
          in  line [a, b, a] ~> Mi2 @?= line [a', b', a']
      , testCase "a scale" $
          let a  = scale $ C#4+|minor <|| def
              a' = scale $ B#4+|minor <|| def
          in  a ~> M7 @?= a'
      , testCase "a sequence of scales" $
          let a  = line $ C#4+|blues <|| 1%8
              a' = line $ Cs#4+|blues <|| 1%8
              b  = line $ Ds#4+|harmonicMinor <|| def
              b' = line $ E#4+|harmonicMinor  <|| def
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
      , testProperty "a diminished chord" $ \n -> n > 0 ==>
          invertN n dim7 == dim7
      , testCase "a scale" $
          mode vi ionian @?= minor
      , testProperty "scale orbit" $ do
          sc <- genScale
          return $ or [invertN n sc == sc | n <- [5..9]]
      , testProperty "chord orbit" $ do
          ch <- genChord
          return $ length ch < 5 ==> or [invertN n ch == ch | n <- [4, 5]]
      ]
  , testGroup "Retro"
      [ testCase "a melody" $
          (line [C#4<|hn, (wn~~), Gs#4<|en] ><) @?=
            line [Gs#4<|en, (wn~~), C#4<|hn]
      , testCase "a chord" $
          (chord (C#4=|maj <||wn) ><) @?= chord (C#4=|maj <||wn)
      , testCase "a scale" $
          (scale (C#4+|major <||sn) ><) @?=
            line (reverse [C, D, E, F, G, A, B]<#4<||sn)
      ]
  , testGroup "Repeat"
      [ testCase "a single note" $
          let note = C#4<|wn
          in  4 ## note @?= note :+: note :+: note :+: note
      , testCase "a piece of music" $
          let piece = line $ chord <$> [c, c', c']
              c = Cs#4=|maj7 <|| def
              c' = db#3=|m7b5 <|| def
          in  3 ## piece @?= piece :+: piece :+: piece
      ]
  , testGroup "Scaling time"
      [ testCase "to smaller single duration" $
          2 *~ hn @?= qn
      , testCase "to bigger single duration" $
          1%2 *~ sn @?= en
      , testCase "a melody" $
          1%4 *~ C#4<|en :+: C#3<|sn @?= C#4<|hn :+: C#3<|qn
      , testCase "a chord" $
          4 *~ chord (C#4=|maj <||wn) @?= chord (C#4=|maj <||qn)
      , testCase "a scale" $
          1%4 *~ scale (eb#2+|bebopDorian <||qn) @?= scale (eb#2=|bebopDorian <||wn)
      ]
  , testGroup "Other"
      [ testCase "toList" $
          musicToList (C#4<|hn :+: (wn~~) :+: C#5<|qn) @?=
            [(Just $ C#4, hn), (Nothing, wn), (Just $ C#5, qn)]
      , testCase "fromList" $
          listToMusic [(Just $ C#4, hn), (Nothing, wn), (Just $ C#5, qn)] @?=
            (C#4<|hn :+: (wn~~) :+: C#5<|qn)
      , testCase "normalize" $
          let m = (wn~~) :: Melody
          in  normalize ((m :+: m) :+: m) @?= m :+: m :+: m
      ]
  ]
