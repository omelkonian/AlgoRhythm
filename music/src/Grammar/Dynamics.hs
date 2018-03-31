{-# LANGUAGE ImplicitParams #-}

module Grammar.Dynamics (addDynamics) where

import           Data.KMeans (kmeansGen)
import           Data.Maybe (fromJust)
import           Data.List (find)
import           Music

type AbsStartTime  = Rational
type PitchIntValue = Int
type ClusterMusicA = (FullPitch, (AbsStartTime, PitchIntValue))
type MusicCluster  = Music ClusterMusicA
type Cluster       = [ClusterMusicA]

addDynamics :: MusicCore -> MusicCore
addDynamics m = do
  let mCluster = coreToCluster m
  let clusters = cluster mCluster
  -- Generate dynamics for notes per cluster, and then concatenate the clusters.
  let dynamics = concatMap addDynamicsToCluster clusters
  addDynamicsToMCore mCluster dynamics

addDynamicsToMCore ::  MusicCluster -> Cluster -> MusicCore
addDynamicsToMCore m c = do
  fmap add m
  where add (_,info) =
            -- Find the element in c with matching absolute start time and pitch int value,
            -- and get the FullPitch (that contains the dynamic) from that element and put
            -- it in the note.
            fst $ fromJust $ find ((info==) . snd) c

minTime :: [ClusterMusicA] -> AbsStartTime
minTime m = (fst . snd) (head (m))

maxTime :: [ClusterMusicA] -> AbsStartTime
maxTime m = (fst . snd) (last (m))

addDynamicsToCluster :: Cluster -> Cluster
addDynamicsToCluster [] = []
addDynamicsToCluster c = do
  let cMin = minTime c
  let cMax = maxTime c
  let cLen = cMax - cMin
  -- Returns a value in [0,1] that indicates how far the Note is in the cluster.
  let progress t = fromRational $ (t - cMin) / cLen
  let pToDyn ((p,attrs),(t,y)) = do
          let maxDynNum = fromIntegral (fromEnum (maxBound :: Dynamic)) :: Double
          let dynNum    = round $ (maxDynNum/2) + ((maxDynNum/4) * (sin (2 * pi * (progress t))))
          let dyn       = Dynamic ((toEnum dynNum) :: Dynamic)
          ((p,dyn:attrs),(t,y))
  map pToDyn c

-- | Clusters a MusicCluster. The number of clusters is equal to half
cluster :: MusicCluster -> [Cluster]
cluster m = kmeansGen gen k (notes m)
  where gen :: ClusterMusicA -> [Double]
        gen (_,(x,y)) = [fromRational x, fromIntegral y]
        -- The number of clusters is equal to the duration of the music divided
        -- by 4 (we assume that 4 beats go into a measure.)
        k = round ((fromRational (duration m) :: Double) / 4)
        -- TODO (if current solution isn't good enough) Better k measure. This one makes no sense whatsoever.
        --k = ceiling $ (fromIntegral (length m')) / (fromRational (maxTime m'))

-- | Assings 2d coordinates to all music Notes (not Rests), where the x is the
--   absolute start time of the Note and the y is the Pitch of the Note
--   represented as a number (in other words, a very abstract representation of
--   the notes on actual sheet music). Also removes PitchAttributes, because
--   they aren't needed here.
coreToCluster :: MusicCore -> MusicCluster
coreToCluster = calcClusterInfo . fmap (\p -> (p,(0,0)) )

-- | Adds absolute times to Notes.
calcClusterInfo :: MusicCluster -> MusicCluster
calcClusterInfo (m1@(Rest l)   :+: m2) = m1                   :+: (fmap (addTime l) (calcClusterInfo m2))
calcClusterInfo (m1@(Note l _) :+: m2) = (calcClusterInfo m1) :+: (fmap (addTime l) (calcClusterInfo m2))
calcClusterInfo (m1 :=: m2)            = (calcClusterInfo m1) :=: (calcClusterInfo m2)
calcClusterInfo (m1 :+: m2)            =
  (calcClusterInfo m1) :+: (fmap (addTime (duration m1)) (calcClusterInfo m2))
calcClusterInfo r@(Rest _)             = r
calcClusterInfo (Note l (p,(x,_)))     = Note l (p,(x, fromEnum p))

-- | Calculates the duration of a piece of Music.
duration :: Music a -> Duration
duration (m1 :+: m2) = (+) (duration m1) (duration m2)
duration (m1 :=: m2) = max (duration m1) (duration m2)
duration (Note l _) = l
duration (Rest l)   = l

-- | Adds an amount of time to the AbsStartTime field of a ClusterMusicA Note.
addTime :: Duration -> ClusterMusicA -> ClusterMusicA
addTime t (p,(x,y)) = (p,(x+t,y))
