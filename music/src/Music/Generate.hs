{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PostfixOperators     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Music.Generate where

import Control.Monad.State
import Test.QuickCheck           (generate)
import Test.QuickCheck.Gen       (elements)

import Export

import Music

-- | A 'Selector' is a function that given a list of elements
--   creates an IO operation that selects one of the elements
--   from the list. This function is required to be polymorphic
--   on the elements of the list.
type Selector = forall b. [b] -> IO b

-- | State to be kept during generation
type Constraint a = a -> Bool  -- TODO add IDs to add/remove
type Entry a = ([a], [Constraint a])
data GenState = GenState { selector :: Selector
                         , pc       :: Entry PitchClass
                         , oct      :: Entry Octave
                         , dur      :: Entry Duration
                         , itv      :: Entry Interval
                         , dyn      :: Entry Dynamic
                         , art      :: Entry Articulation
                         }

-- | A 'Music' generator is simply state monad wrapped around IO.
--   Note that this type is eta reduced and that it takes one parameter
--   to describe the result type of the generation.
type MusicGenerator = StateT GenState IO

-- | Default state to start any generation with. This effectively means
--   that no constraints are applied and that the selector is just a function
--   that picks a random element from a list.
defaultState :: GenState
defaultState = GenState { selector = generate . elements
                        , pc  = ([C ..], [])
                        , oct = ([Oct0 ..], [])
                        , dur = ([1%16,1%8,1%4,1%2], [])
                        , itv = ([P1 ..], [])
                        , dyn = ([PPPPP ..], [])
                        , art = ([Staccato ..], [])
                        }

constrain :: Entry a -> [a]
constrain (xs, cs) = filter (\x -> all ($ x) cs) xs

(??) :: (GenState -> ([a], [Constraint a])) -> MusicGenerator a
(??) accessor = do
  st <- get
  io $ selector st (constrain $ accessor st)

class Generatable a where
  rand :: MusicGenerator a

  randN :: Int -> MusicGenerator [a]
  randN n = replicateM n rand

instance Generatable PitchClass where
  rand = (pc??)
instance Generatable Octave where
  rand = (oct??)
instance Generatable Duration where
  rand = (dur??)

instance Generatable Pitch where
  rand = (,) <$> rand <*> rand

-- | Generate a note within the currently applied constraints.
genNote :: MusicGenerator Melody
genNote = (<|) <$> rand <*> rand

genChord :: Int -> MusicGenerator Melody
genChord n =
  chord <$> (map <$> (Note <$> rand)
                 <*> (zip <$> randN n <*> randN n))

genConstraintedChord :: MusicGenerator Melody
genConstraintedChord = do
  -- Add constraints
  modify (\st -> st { pc = (fst (pc st), [inC]) }) -- Chords in Cmaj
  modify (\st -> st { dur = (fst (dur st), [inD]) }) -- Simple durations
  -- Generate constrainted chord
  genChord 4
  where inC p = p `elem` [A, C, Ds, Fs]
        inD d = d `elem` [en, sn, tn]

-- | Lift an IO operation into the 'MusicGenerator' monad.
io :: IO a -> MusicGenerator a
io = liftIO

-- | Runs a generator on the default state.
runGenerator :: MusicGenerator a -> IO a
runGenerator = runGenerator' defaultState

-- | Runs a generator on the provided state
runGenerator' :: GenState -> MusicGenerator a -> IO a
runGenerator' st gen = fst <$> runStateT gen st

playGen :: ToMusicCore a => MusicGenerator (Music a) -> IO ()
playGen music = do
  m <- runGenerator music
  playDev 4 m
