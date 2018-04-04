{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PostfixOperators       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Generate.Generate where

import Control.Monad.State hiding (state)
import Music

type Weight = Double
type Selector s a = s -> [(Weight, a)] -> IO (a, s)

data Accessor st s a = Accessor
  { getValue :: st s -> Entry s a
  , setValue :: Entry s a -> st s -> st s
  }

-- | State to be kept during generation
type Constraint a = a -> Bool

data Entry s a = Entry { values      :: [(Weight, a)]
                       , constraints :: [Constraint a]
                       , selector    :: Selector s a
                       }

data GenState s = GenState { state     :: s
                           , pc    :: Entry s PitchClass
                           , oct   :: Entry s Octave
                           , dur   :: Entry s Duration
                           , itv   :: Entry s Interval
                           , dyn   :: Entry s Dynamic
                           , art   :: Entry s Articulation
                           }

pitchClass   :: Accessor GenState s PitchClass
pitchClass   = Accessor { getValue = pc , setValue = \e st -> st { pc  = e } }

octave       :: Accessor GenState s Octave
octave       = Accessor { getValue = oct, setValue = \e st -> st { oct = e } }

duration     :: Accessor GenState s Duration
duration     = Accessor { getValue = dur, setValue = \e st -> st { dur = e } }

interval     :: Accessor GenState s Interval
interval     = Accessor { getValue = itv, setValue = \e st -> st { itv = e } }

dynamic      :: Accessor GenState s Dynamic
dynamic      = Accessor { getValue = dyn, setValue = \e st -> st { dyn = e } }

articulation :: Accessor GenState s Articulation
articulation = Accessor { getValue = art, setValue = \e st -> st { art = e } }

-- | A 'Music' generator is simply state monad wrapped around IO.
type MusicGenerator s a = GenericMusicGenerator GenState s a

type GenericMusicGenerator st s a = StateT (st s) IO a

getEntry :: Accessor st s a -> GenericMusicGenerator st s (Entry s a)
getEntry accessor = do
  st <- get
  return $ getValue accessor st

(?@) :: Accessor st s a -> GenericMusicGenerator st s (Entry s a)
(?@) = getEntry

putEntry :: Accessor st s a -> Entry s a -> GenericMusicGenerator st s ()
putEntry accessor entry = modify $ setValue accessor entry

(>@) :: Accessor st s a -> Entry s a -> GenericMusicGenerator st s ()
(>@) = putEntry

putSelector :: Accessor st s a -> Selector s a -> GenericMusicGenerator st s ()
putSelector accessor sel = do
  entry <- getEntry accessor
  putEntry accessor (entry { selector = sel })

(>?) :: Accessor st s a -> Selector s a -> GenericMusicGenerator st s ()
(>?) = putSelector

putOptions :: Accessor st s a -> [(Weight, a)] -> GenericMusicGenerator st s ()
putOptions accessor options = do
  entry <- getEntry accessor
  putEntry accessor (entry { values = options })

(>+) :: Accessor st s a -> [(Weight, a)] -> GenericMusicGenerator st s ()
(>+) = putOptions

setState :: s -> MusicGenerator s ()
setState state' = modify (\st -> st { state = state' })

(.#.) :: (Applicative m) => Int -> m a -> m [a]
(.#.) = replicateM


(>$) :: s -> MusicGenerator s ()
(>$) = setState

select :: Accessor GenState s a -> MusicGenerator s a
select = gselect state setState

gselect :: (st s -> s)
        -> (s -> GenericMusicGenerator st s ())
        -> Accessor st s a
        -> GenericMusicGenerator st s a
gselect stateGet stateSet accessor = do
  e <- getEntry accessor
  genstate <- get
  let st  = stateGet genstate
  let e'  = constrain e
  let sel = selector e
  (value, st') <- lift (sel st e')
  stateSet st'
  return value

constrain :: Entry s a -> [(Weight, a)]
constrain e = filter (\(_, x) -> all ($ x) (constraints e)) $ values e

addConstraint :: Accessor st s a -> Constraint a -> GenericMusicGenerator st s ()
addConstraint accessor c = do
  e <- getEntry accessor
  putEntry accessor Entry { values      = values e
                          , constraints = c:constraints e
                          , selector    = selector e
                          }

(>!) :: Accessor st s a -> Constraint a -> GenericMusicGenerator st s ()
(>!) = addConstraint

(??) :: Accessor GenState s a -> MusicGenerator s a
(??) = select

class Generatable st a where
  rand :: GenericMusicGenerator st s a

  randN :: Int -> GenericMusicGenerator st s [a]
  randN n = replicateM n rand

instance Generatable GenState PitchClass where
  rand = (pitchClass??)
instance Generatable GenState Octave where
  rand = (octave??)
instance Generatable GenState Duration where
  rand = (duration??)

instance Generatable GenState Pitch where
  rand = (,) <$> rand <*> rand

-- | Generate a note within the currently applied constraints.
genNote :: MusicGenerator s Melody
genNote = (<|) <$> rand <*> rand

genChord :: Int -> MusicGenerator s Melody
genChord n =
  chord <$> (map <$> (Note <$> rand)
                 <*> (zip <$> randN n <*> randN n))
Scale
-- | Runs a generator on the provided state
runGenerator' :: st s -> GenericMusicGenerator st s a -> IO a
runGenerator' st gen = fst <$> runStateT gen st

modified :: (st s -> st s)
         -> GenericMusicGenerator st s a
         -> GenericMusicGenerator st s a
modified f gen = get >>= \st -> lift $ runGenerator' (f st) gen

local :: GenericMusicGenerator st s a -> GenericMusicGenerator st s a
local = modified id
