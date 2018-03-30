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

addDynamics :: MusicCore -> Duration -> MusicCore
addDynamics m dur = do
  let mCluster = coreToCluster m
  let clusters = cluster mCluster dur
  -- Generate dynamics for notes per cluster, and then concatenate the clusters.
  let dynamics = concatMap addDynamicsToCluster clusters
  addDynamicsToMCore mCluster dynamics

addDynamicsToMCore ::  MusicCluster -> Cluster -> MusicCore
addDynamicsToMCore (m1 :+: m2) cs = (addDynamicsToMCore m1 cs :+: addDynamicsToMCore m2 cs)
addDynamicsToMCore (m1 :=: m2) cs = (addDynamicsToMCore m1 cs :=: addDynamicsToMCore m2 cs)
addDynamicsToMCore (Rest d)  _  = Rest d
addDynamicsToMCore (Note d (_,info)) cs = do
  -- Find the element in cs with matching absolute start time and pitch int value,
  -- and get the FullPitch (that contains the dynamic) from that element and put
  -- it in the note.
  let (p,_) = fromJust $ find ((info==) . snd) cs
  Note d p

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

cluster :: MusicCluster -> Duration -> [Cluster]
cluster m dur = kmeansGen gen k m'
  where gen :: ClusterMusicA -> [Double]
        gen (_,(x,_)) = [fromRational x]--, fromIntegral y]
        m' = foldr (:) [] m
        k = round (((fromRational dur) / 2) :: Double)
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
calcClusterInfo (m1@(Rest l)   :+: m2) = m1 :+: (fmap (addTime l) (calcClusterInfo m2))
calcClusterInfo (m1@(Note l _) :+: m2) = (calcClusterInfo m1) :+: (fmap (addTime l) (calcClusterInfo m2))
calcClusterInfo (m1 :=: m2)            = (calcClusterInfo m1) :=: (calcClusterInfo m2)
calcClusterInfo r@(Rest _)             = r
calcClusterInfo (Note l (p,(x,_)))     = Note l (p,(x, fromEnum p))
-- TODO: Add missing patterns. I don't know if we have to though, because it's
--       only necessary if the used grammars can create something like
--       (a :+: b) :+: c instead of a :+: (b :+: c).

addTime :: Rational -> ClusterMusicA -> ClusterMusicA
addTime t (p,(x,y)) = (p,(x+t,y))
