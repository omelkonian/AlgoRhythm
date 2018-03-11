{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TupleSections              #-}

module Music.Generate where

import           Test.QuickCheck           (generate, choose)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen       (elements)
import           Control.Monad.State
import           Data.DeriveTH
import           Data.Maybe

import qualified Data.Map.Strict           as M

import           Music

-- | Automatically derive 'Arbitrary' instances.
derive makeArbitrary ''PitchClass
derive makeArbitrary ''Octave
derive makeArbitrary ''Music
derive makeArbitrary ''Interval

-- | Generic datatype that the degrees of freedom within
--   a piece of music
data MusicElement =
    Art Articulation
  | Dyn Dynamic
  | Pit PitchClass
  | Dur Duration
  | Oct Octave
  | Itv Interval

-- | A 'Selector' is a function that given a list of elements
--   creates an IO operation that selects one of the elements
--   from the list. This function is required to be polymorphic
--   on the elements of the list.
type Selector = forall b . [b] -> IO b

-- | State to be kept during generation
data GenState where
  GenState :: Selector -> M.Map String [MusicElement] -> GenState

-- | A 'Music' generator is simply state monad wrapped around IO.
--   Note that this type is eta reduced and that it takes one parameter
--   to describe the result type of the generation.
type MusicGenerator = StateT GenState IO

-- | Default state to start any generation with. This effectively means
--   that no constraints are applied and that the selector is just a function
--   that picks a random element from a list.
defaultState :: GenState
defaultState = GenState
  (generate . elements)
  (M.fromList
    [
      ("pitchClass", map Pit [C ..]),
      ("octave", map Oct [Oct0 ..]),
      ("duration", map Dur [1%16,1%8,1%4,1%2]),
      ("articulation", map Art [Staccato ..]),
      ("dynamic", map Dyn [PPPPP ..]),
      ("interval", map Itv [P1 ..])
    ]
  )

-- | Gets all possible options for a certain element given the
--   constraints that are currently applied.
options :: String -> MusicGenerator [MusicElement]
options key = do
  (GenState _ m) <- get
  case M.member key m of
    True  -> return $ fromJust (M.lookup key m)
    False -> error "Tried to fetch options for unknown element"

-- | Select a random element from the list found at the given key
--   using the selector that is provided to the generator.
value :: String -> MusicGenerator MusicElement
value key = do
  (GenState s _) <- get
  xs             <- options key
  io (s xs)

genArticulation :: MusicGenerator Articulation
genArticulation = do
  (Art x) <- value "articulation"
  return x

genDynamic :: MusicGenerator Dynamic
genDynamic = do
  (Dyn x) <- value "dynamic"
  return x

genPitch :: MusicGenerator PitchClass
genPitch = do
  (Pit x) <- value "pitchClass"
  return x

genDuration :: MusicGenerator Duration
genDuration = do
  (Dur x) <- value "duration"
  return x

genOctave :: MusicGenerator Octave
genOctave = do
  (Oct x) <- value "octave"
  return x

genInterval :: MusicGenerator Interval
genInterval = do
  (Itv x) <- value "interval"
  return x

-- | Generate a note within the currently applied constraints.
genNote :: MusicGenerator Melody
genNote = do
  (Pit p) <- value "pitchClass"
  (Oct o) <- value "octave"
  (Dur d) <- value "duration"
  return ((p, o) <| d)

genChord :: Int -> MusicGenerator Melody
genChord n = do
  octaves  <- replicateM n genOctave
  pitches  <- replicateM n genPitch
  duration <- genDuration
  return $ chord $ map (Note duration) (zip pitches octaves)

-- | Lift an IO operation into the 'MusicGenerator' monad.
io :: IO a -> MusicGenerator a
io = liftIO

-- | Runs a generator on the default state.
runGenerator :: MusicGenerator a -> IO a
runGenerator = runGenerator' defaultState

-- | Runs a generator on the provided state
runGenerator' :: GenState -> MusicGenerator a -> IO a
runGenerator' st gen = runStateT gen st >>= return . fst
