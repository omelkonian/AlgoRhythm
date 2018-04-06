{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TScore where

import qualified Data.Music.Lilypond            as Ly
import           Data.Ratio
import           System.Directory               (doesFileExist, removeFile)
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Random                  (newStdGen, randomRs)
import           Test.Framework                 (Test, buildTestBracketed,
                                                 testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=))

import Export
import Grammar
import Music


testAndCleanup :: (String -> Test) -> Test
testAndCleanup t = buildTestBracketed $ do
  g     <- newStdGen
  let f = take 8 (randomRs ('a','z') g) ++ ".ly"
  let test' = t f
  let cleanup = removeFile f
  return (test', cleanup)

scoreTests :: Test
scoreTests = testGroup "Score"
  [ testAndCleanup $ \t -> testCase "successfully write to file" $ do
      let res = do let ?harmonyConfig = defHarmonyConfig
                   let ?melodyConfig = defMelodyConfig
                   let ?tablaBeat = sn
                   m <- runGrammar tabla wn ()
                   -- (back, fore) <- integrate (4 * wn)
                   _ <- writeToLilypondFile t m
                   doesFileExist t
      unsafePerformIO res @?= True,
    testCase "Split a note duration into powers of 2" $
      splitDurations (11 % 16) @?= [1%2, 1%8, 1%16],
    testCase "Correctly tie notes while generating score" $
      musicToLilypond ((C#4 <: []) <| (11%16)) @?=
        Ly.Sequential
        [
          Ly.Note (
            Ly.NotePitch
              Ly.Pitch {Ly.getPitch = (Ly.C,0,5)}
              Nothing)
            (Just
              Ly.Duration
                {Ly.getDuration = 1 % 2}
            )
          [Ly.Tie],
          Ly.Note (
            Ly.NotePitch
              Ly.Pitch {Ly.getPitch = (Ly.C,0,5)}
              Nothing)
            (Just
              Ly.Duration
                {Ly.getDuration = 1 % 8}
            )
          [Ly.Tie],
          Ly.Note (
            Ly.NotePitch
              Ly.Pitch
                {Ly.getPitch = (Ly.C,0,5)}
              Nothing)
            (Just
              Ly.Duration
                {Ly.getDuration = 1 % 16}
            )
          []
        ]
  ]
