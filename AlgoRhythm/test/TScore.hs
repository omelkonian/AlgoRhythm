{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TScore where

import qualified Data.Music.Lilypond            as Ly
import           Data.Ratio
import           System.Directory               (doesFileExist)
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

import Export
import Grammar
import Music

scoreTests = testGroup "Score"
  [ testCase "successfully write to file" $
      let res = do let f = "test.ly"
                   let ?harmonyConfig = defHarmonyConfig
                   let ?melodyConfig = defMelodyConfig
                   (back, fore) <- integrate (2 * wn) -- TODO larger pieces eat up RAM :(
                   _ <- writeToLilypondFile f (back :=: fore)
                   doesFileExist f
      in  unsafePerformIO res @?= True,
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
