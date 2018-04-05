module Dynamics ( addDynamics
                , dyn
                , sinTimeDynamics
                , sinPitchDynamics
                , expPitchDynamics
                , DynamicsMap) where

import Data.KMeans (kmeansGen)
import Data.List   (find)
import Data.Maybe  (fromJust)
import Music

type AbsStartTime  = Rational
type PitchIntValue = Int
type ClusterMusicA = (FullPitch, (AbsStartTime, PitchIntValue))
type MusicCluster  = Music ClusterMusicA
type Cluster       = [ClusterMusicA]

-- | Gets the time of the note within its cluster as a Double in the range
--   [0,1] (where 0 represents that the note is at the start of the cluster,
--   and 1 that it's at the end of the cluster) and the pitch of the note
--   within the cluster (where 0 represents the lowest note in the cluster and
--   1 the highest note in the cluster) and returns another Double in the range
--   [0,1] that represents how loud the note should be played, where 0 is as
--   soft as possible (but not silent!) and 1 is as loud as possible.
type DynamicsMap =  Double -- ^ Time location of note in cluster, in range [0,1].
                 -> Double -- ^ Pitch location in cluster, in range [0,1].
                 -> Double -- ^ Volume of the note, in range [0,1].

-- | Returns a dynamic between 25% and 75% volume, based on one full sine
--   oscillation
sinTimeDynamics :: DynamicsMap
sinTimeDynamics x _ = 0.25 + (0.25 * sin (2 * pi * x))

-- | Returns a dynamic between 0% and 100% volume, based on one full sine
--   oscillation. Note that this sounds ridiculous
sinPitchDynamics :: DynamicsMap
sinPitchDynamics x _ = 0.25 + (0.25 * sin (2 * pi * x))

-- | Returns a dynamic between 0% and 100% volume, based on the exponential
--   quantile function and the relative pitch height of the note in the cluster.
expPitchDynamics :: DynamicsMap
expPitchDynamics _ y = max 0.45 (min 0.8 (-log (1 - y)))

-- | Adds `Dynamic` to all notes in the given `Music` using `expPitchDynamics`.
dyn :: (ToMusicCore a) => Music a -> MusicCore
dyn m = addDynamics m expPitchDynamics

-- | Adds `Dynamic` to all notes in the given `Music` using the given `DynamicsMap`
addDynamics :: (ToMusicCore a) => Music a -> DynamicsMap -> MusicCore
addDynamics m' dynMap = do
  let m = toMusicCore m'
  let mCluster = coreToCluster m
  let clusters = cluster mCluster
  -- Generate dynamics for notes per cluster, and then concatenate the clusters.
  let dynamics = concatMap (addDynamicsToCluster dynMap) clusters
  addDynamicsToMCore mCluster dynamics

addDynamicsToMCore ::  MusicCluster -> Cluster -> MusicCore
addDynamicsToMCore m c =
  fmap add m
  where add (_,info) =
            -- Find the element in c with matching absolute start time and pitch int value,
            -- and get the FullPitch (that contains the dynamic) from that element and put
            -- it in the note.
            fst $ fromJust $ find ((info==) . snd) c

bounds :: [ClusterMusicA] -> ((Double, Double),(Double, Double))
bounds m = ( (fromRational (minimum (map (fst . snd) m)), fromIntegral (minimum (map (snd . snd) m)))
           , (fromRational (maximum (map (fst . snd) m)), fromIntegral (maximum (map (snd . snd) m)))
           )

addDynamicsToCluster :: DynamicsMap -> Cluster -> Cluster
addDynamicsToCluster _ [] = []
addDynamicsToCluster f c  = do
  let ((minTime,minNote),(maxTime,maxNote)) = bounds c
  -- Returns a value in [0,1] that indicates how far the Note is in the cluster.
  let tProg t = ((fromRational t) - minTime) / (maxTime - minTime)
  -- Returns a value in [0,1] that indicates how high the Note is in the cluster.
  let pProg n = ((fromIntegral n) - minNote) / (maxNote - minNote)
  let pToDyn ((p',attrs),(t,p)) = do
          let dynDouble = f (tProg t) (pProg p)
          if dynDouble < 0 || dynDouble > 1 then
            error "Result from DynamicsMap is not in range [0,1]."
          else do
            let maxDynNum = fromIntegral (fromEnum (maxBound :: Dynamic)) :: Double
            let dynNum    = round (maxDynNum * dynDouble)
            let d       = Dynamic (toEnum dynNum :: Dynamic)
            ((p',d:attrs),(t,p))
  map pToDyn c

-- | Clusters a MusicCluster. The number of clusters is equal to half
cluster :: MusicCluster -> [Cluster]
cluster m = kmeansGen gen k (notes m)
  where gen :: ClusterMusicA -> [Double]
        gen (_,(x,y)) = [fromRational x, fromIntegral y]
        -- The number of clusters is equal to the duration of the music divided
        -- by 4 (we assume that 4 beats go into a measure.)
        k = max 1 (round ((fromRational (duration m) :: Double) / 4))

-- | Assings 2d coordinates to all music Notes (not Rests), where the x is the
--   absolute start time of the Note and the y is the Pitch of the Note
--   represented as a number (in other words, a very abstract representation of
--   the notes on actual sheet music). Also removes PitchAttributes, because
--   they aren't needed here.
coreToCluster :: MusicCore -> MusicCluster
coreToCluster = calcClusterInfo . fmap (\p -> (p,(0,0)) )

-- | Adds absolute times to Notes.
calcClusterInfo :: MusicCluster -> MusicCluster
calcClusterInfo (m1@(Rest l)   :+: m2) = m1                 :+: fmap (addTime l) (calcClusterInfo m2)
calcClusterInfo (m1@(Note l _) :+: m2) = calcClusterInfo m1 :+: fmap (addTime l) (calcClusterInfo m2)
calcClusterInfo (m1 :=: m2)            = calcClusterInfo m1 :=: calcClusterInfo m2
calcClusterInfo (m1 :+: m2)            =
  calcClusterInfo m1 :+: fmap (addTime (duration m1)) (calcClusterInfo m2)
calcClusterInfo r@(Rest _)             = r
calcClusterInfo (Note l (p,(x,_)))     = Note l (p,(x, fromEnum p))

-- | Calculates the duration of a piece of Music.
duration :: Music a -> Duration
duration (m1 :+: m2) = (+) (duration m1) (duration m2)
duration (m1 :=: m2) = max (duration m1) (duration m2)
duration (Note l _)  = l
duration (Rest l)    = l

-- | Adds an amount of time to the AbsStartTime field of a ClusterMusicA Note.
addTime :: Duration -> ClusterMusicA -> ClusterMusicA
addTime t (p,(x,y)) = (p,(x+t,y))
