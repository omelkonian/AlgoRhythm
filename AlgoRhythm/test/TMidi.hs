{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TMidi where

import           Codec.Midi                     (importFile)
import           Control.Applicative            ((<|>))
import qualified Data.ByteString                as B
import           Data.Char                      (toUpper)
import           Data.List                      (find, intersect, sort)
import qualified Euterpea                       as E
import           Euterpea.IO.MIDI               (fromMidi)
import           System.Directory               (doesFileExist, removeFile)
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Random                  (newStdGen, randomRs)
import           Test.Framework                 (Test, buildTestBracketed,
                                                 testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=), Assertion)
import           Text.Printf                    (printf)

import Export
import Grammar hiding ((<|>))
import Music

-- | Generates a random filename `f` with the .midi extension, runs the given
--   test `t` using that filename, and immediately removes the file stored at
--   location `f` after the test finished. The reason that we have to generate
--   random file names and cannot use the same one all the time is that tests
--   can be exectued concurrently.
testAndCleanup :: (String -> Test) -> Test
testAndCleanup t = buildTestBracketed $ do
  g     <- newStdGen
  let f = take 8 (randomRs ('a','z') g) ++ ".midi"
  let test = t f
  let cleanup = removeFile f
  return (test, cleanup)

midiTests :: Test
midiTests = testGroup "MIDI export"
  [ testAndCleanup $ \f -> testCase "Successfully write to file" $ do
      let res = unsafePerformIO $ do
                  let ?harmonyConfig = defHarmonyConfig
                  let ?melodyConfig = defMelodyConfig
                  let ?midiConfig = defaultMIDIConfig
                  (back, fore) <- integrate (16 * wn)
                  writeToMidiFile f (back :=: fore)
                  doesFileExist f
      res @?= True

  -- Check if the header is correct (HCodecs (which is used by Euterpea))
  -- doesn't check MIDI headers properly.
  , testAndCleanup $ \f -> testCase "Correct Midi header" $ do
      {- See: https://www.csie.ntu.edu.tw/~r92092/ref/midi/

         4D546864     = "MThd", which represents the start of a MIDI header chunk.
         00000006     = length of the actual header chunk. This is always 6 bytes.
         000100000060 = The six byte long header chunk. Can be subdivided in:
           0001       = MIDI file format. Should be 0,1 or 2. 1 means that there
                        is only 1 track and that everything is played concurrently.
           0000       = The number of track chunks. Should be 0 obviously, since
                        our music only consists of a rest.
           0060       = Speed. hex 0060 = bin 0-000000001100000. Here, the most
                        significant bit says that the unit of speed is ticks per
                        quarternote. The last 15 bits are the number of ticks,
                        so 96 in decimal.

      -}
      let midiHex = "4D546864-00000006-000100000060"
      let m = Rest 0 :: Music Chord
      let byteString = unsafePerformIO $ do
                                  let ?midiConfig = defaultMIDIConfig
                                  writeToMidiFile f m
                                  B.readFile f
      let hex        = concatMap (printf "%02x") (B.unpack byteString)
      let upperHex   = map toUpper hex
      upperHex @?= filter ('-'/=) midiHex

  , testCase "Sequential music to Euterpea" $ do
      let ?midiConfig = MIDIConfig (1%2) [AcousticGrandPiano]
      let m = toMusicCore $ C#4<|qn :+: Cs#3<|hn
      let mE = musicToE m
      let mEExpected = E.Modify (E.Tempo (1 % 2)) (
                         E.Modify (E.Instrument AcousticGrandPiano) (
                           E.Prim (E.Note (1 % 4) ((E.C,4),[]))
                           E.:+:
                           E.Prim (E.Note (1 % 2) ((E.Cs,3),[]))
                         )
                       )
      mE @?= mEExpected

  , testCase "Parallel music to Euterpea" $ do
      let ?midiConfig = MIDIConfig (1%4) [Piccolo]
      let m = toMusicCore $ G#1<|qn :=: Ds#6<|hn
      let mE = musicToE m
      let mEExpected = E.Modify (E.Tempo (1 % 4)) (E.Modify (E.Instrument Piccolo) (
                         E.Prim (E.Note (1 % 4) ((E.G,1),[]))
                       ))
                         E.:=:
                       E.Modify (E.Tempo (1 % 4)) (E.Modify (E.Instrument Piccolo) (
                         E.Prim (E.Note (1 % 2) ((E.Ds,6),[]))
                       ))
      mE @?= mEExpected

  , testAndCleanup $ \f -> testCase "Sequential music to Midi and back" $ do
      let ?midiConfig = defaultMIDIConfig
      let m = toMusicCore $ C#4<|qn :+: Cs#3<|hn
      let mE1 = musicToE m
      unsafePerformIO $ do
        writeToMidiFile f m
        mE2 <- importFile f >>= \(Right m') -> return (fromMidi m')
        return $ compareMusic1s (preprocess mE1) (preprocess (preprocess mE2))

  , testAndCleanup $ \f -> testCase "Parallel music to Midi and back" $ do
      let ?midiConfig = MIDIConfig 1 [AcousticGrandPiano, Banjo]
      let m = toMusicCore $ G#1<|qn :=: Ds#6<|hn
      let mE1 = musicToE m
      unsafePerformIO $ do
        writeToMidiFile f m
        mE2 <- importFile f >>= \(Right m) -> return (fromMidi m)
        return $ compareMusic1s (preprocess mE1) (preprocess (preprocess mE2))
  ]

-- | Rewrites the Music1 that was read from a MIDI file, preprocesses it,
--   permutes it, e.g. a :=: b is the same as b :=: a, and checks if there is at
--   least 1 permutation that is exactly equal to the original Music1.
compareMusic1s :: E.Music1 -> E.Music1 -> Assertion
compareMusic1s mOriginal mRead = do
  let mOriginalMs = commonModifiers [] mOriginal
  let mOriginal'   = stripModifiers mOriginalMs mOriginal
  let mReadMs     = commonModifiers [] mRead
  let mRead'      = stripModifiers mReadMs mRead
  -- remove e.g. empty rests in iteration 1, rewrite in iteration 2.
  let mReadPreprocessed = preprocess $ preprocess mRead'
  let mReadPerms        = perms mReadPreprocessed
  let (Just p)          = find (mOriginal'==) mReadPerms <|> Just (head mReadPerms)
  (p, sort mReadMs) @?= (mOriginal', sort mOriginalMs)

commonModifiers :: [E.Control] -> E.Music1 -> [E.Control]
commonModifiers cs (E.Modify c m) = commonModifiers (c:cs) m
commonModifiers cs (a E.:=: b)    = commonModifiers cs a `intersect` commonModifiers cs b
commonModifiers cs (a E.:+: b)    = commonModifiers cs a `intersect` commonModifiers cs b
commonModifiers cs _            = cs

stripModifiers :: [E.Control] -> E.Music1 -> E.Music1
stripModifiers cs (E.Modify c m) | elem c cs = stripModifiers cs m
stripModifiers cs (E.Modify c m) | otherwise = E.Modify c (stripModifiers cs m)
stripModifiers cs (a E.:=: b)    = stripModifiers cs a E.:=: stripModifiers cs b
stripModifiers cs (a E.:+: b)    = stripModifiers cs a E.:+: stripModifiers cs b
stripModifiers _   x             = x

perms :: E.Music1 -> [E.Music1]
perms (m1 E.:=: m2) = concatMap (\(m1',m2') -> [m1' E.:=: m2', m2' E.:=: m1']) (perms' m1 m2)
perms (m1 E.:+: m2) = concatMap (\(m1',m2') -> [m1' E.:+: m2']) (perms' m1 m2)
perms (E.Modify x (E.Modify y m)) = concatMap ops (perms m)
  where ops m' = [E.Modify x (E.Modify y m'), E.Modify y (E.Modify x m')]
perms (E.Modify x m) = map (E.Modify x) (perms m)
perms prim = [prim]

perms' :: E.Music1 -> E.Music1 -> [(E.Music1, E.Music1)]
perms' m1 m2 = [(m1',m2') | m1'<-perms m1, m2' <-perms m2]

-- | The data read from file is slightly differently formatted (rests with
--   duration 0 and some other stuff, so the preprocess function is called
--   on the Music1 that was generated by reading from a midi file, before
--   the Music1 can be compared to the original Music1.)
preprocess :: E.Music1 -> E.Music1
preprocess (E.Modify x m) = E.Modify x (preprocess m)
preprocess (n@(E.Prim (E.Note l1 _)) E.:=: (E.Prim (E.Rest l2) E.:+: m))
  | l1 == l2 = (preprocess n) E.:+: (preprocess m)
  | otherwise = (preprocess n) E.:=: (E.Prim (E.Rest l2) E.:+: (preprocess m))
preprocess (r@(E.Prim (E.Rest l)) E.:+: m) = if l == 0 then preprocess m else (r E.:+: (preprocess m))
preprocess (m E.:+: r@(E.Prim (E.Rest l))) = if l == 0 then preprocess m else ((preprocess m) E.:+: r)
preprocess (r@(E.Prim (E.Rest l)) E.:=: m) = if l == 0 then preprocess m else (r E.:=: (preprocess m))
preprocess (m E.:=: r@(E.Prim (E.Rest l))) = if l == 0 then preprocess m else ((preprocess m) E.:+: r)
preprocess (m1 E.:+: m2) = preprocess m1 E.:+: preprocess m2
preprocess (m1 E.:=: m2) = preprocess m1 E.:=: preprocess m2
preprocess (E.Prim (E.Rest l)) = E.Prim (E.Rest l)
preprocess (E.Prim (E.Note l (x, xs))) = E.Prim (E.Note l (x, filter notVol xs))
  where notVol (E.Volume _) = False
        notVol _            = True
