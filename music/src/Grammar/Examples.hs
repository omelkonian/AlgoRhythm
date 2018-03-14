{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Grammar.Examples where

import Control.Monad (forM)
import System.Random

import Grammar.Grammar
import Grammar.RandomUtils
import Music

---------------------------------- Harmony -------------------------------------
data Degree = I | II | III | IV | V | VI | VII
              deriving (Eq, Show, Enum, Bounded)

newtype Modulation = Modulation Interval deriving (Eq)

harmony :: Grammar Degree Modulation
harmony =
  [ -- Turn-arounds
    (I, 7, (> hn)) :-> \t -> II%:t/4 :-: V%:t/4 :-: I%:t/2
  , (I, 2, (> hn)) :-> \t -> V%:t/2 :-: I%:t/2
  , I -| 1
    -- TODO ++

    -- Modulations
  , (V, 6, (> hn)) :-> \t -> Modulation P5 $: I%:t
  , V -| 3
  -- , (II, 3, (> hn)) :-> \t -> Modulation M2 $: I%:t
  -- , II -| 7
    -- TODO ++

    -- Tritone substitution
  , (V, 1, always) :-> \t -> Modulation A4 |$: V%:t
    -- TODO ++
  ]

interpret :: PitchClass -> Degree -> IO SemiChord
interpret pc degree = do
  let tonic = pc +| major :: SemiScale
  let tone = tonic !! fromEnum degree
  let options = [ ch
                | chordType <- allChords
                , let ch = tone =| chordType
                , all (`elem` tonic) ch
                ]
  index <- getStdRandom $ randomR (0, length options - 1)
  return $ options !! index

instance Expand PitchClass Degree Modulation SemiChord where
  expand pc (Prim (a, t)) = do
    ch <- pc `interpret` a
    return $ Prim (ch, t)
  expand pc (m :-: m') = (:-:) <$> expand pc m <*> expand pc m'
  expand pc (Aux _ (Modulation itv) t) = expand (pc ~~> itv) t

---------------------------------- Melody -------------------------------------
data NT = MQ -- Meta-rhythm
        | Q  -- Rhythm non-terminal
        | MN -- Meta-tone
        | N  -- Note non-terminal
        | HT -- any of [CT, L, AT]
        | CT -- chord tone
        | L  -- color tone
        | AT -- approach tone
        | ST -- scale tone
        | R  -- rest
        deriving (Eq, Show)

melody :: Grammar NT ()
melody =
  [ -- Rhythm { expand MQ(*) to multiple Q(wn), Q(hn) and Q(qn) }
    (MQ, 1, (== 0))      |-> R%:0
  , (MQ, 1, (== qn))     |-> Q%:qn
  , (MQ, 1, (== hn))     |-> Q%:hn
  , (MQ, 1, (== (hn^.))) |-> Q%:hn :-: Q%:qn
  , (MQ, 25, (> (hn^.)))  :-> \t -> Q%:hn :-: MQ%:(t - hn)
  , (MQ, 75, (> wn))      :-> \t -> Q%:wn :-: MQ%:(t - wn)

    -- Melody { expand Qs to notes }
  , (Q, 52, (== wn)) |-> Q%:hn :-: MN%:qn :-: MN%:qn
  , (Q, 47, (== wn)) |-> MN%:qn :-: Q%:hn :-: MN%:qn
  , (Q,  1, (== wn)) |-> MN%:en :-: N%:qn :-: N%:qn :-: N%:qn :-: MN%:en

  , (Q, 60, (== hn)) |-> MN%:qn :-: MN%:qn
  , (Q, 16, (== hn)) |-> HT%:(qn^.) :-: N%:en
  , (Q, 12, (== hn)) |-> MN%:en :-: N%:qn :-: MN%:en
  , (Q,  6, (== hn)) |-> N%:hn
  , (Q,  6, (== hn)) |-> HT%:(qn^^^) :-: HT%:(qn^^^) :-: HT%:(qn^^^)

  , (Q, 1, (== qn)) |-> CT%:qn

  , (MN, 72, (== qn)) |-> MN%:en :-: MN%:en
  , (MN, 22, (== qn)) |-> N%:qn
  , (MN,  5, (== qn)) |-> HT%:(en^^^) :-: HT%:(en^^^) :-: HT%:(en^^^)
  , (MN,  1, (== qn)) |-> HT%:(en^^^) :-: HT%:(en^^^) :-: AT%:(en^^^)

  , (MN, 99, (== en)) |-> N%:en
  , (MN,  1, (== en)) |-> HT%:sn :-: AT%:sn

  , (N, 1, (== hn)) |-> CT%:hn

  , (N, 50, (== qn)) |-> CT%:qn
  , (N, 50, (== qn)) |-> ST%:qn
  , (N, 25, (== qn)) |-> R%:qn
  , (N, 20, (== qn)) |-> L%:qn
  , (N,  1, (== qn)) |-> AT%:qn

  , (N, 40, (== en)) |-> CT%:en
  , (N, 40, (== en)) |-> ST%:en
  , (N, 20, (== en)) |-> L%:en
  , (N, 10, (== en)) |-> R%:en
  , (N,  1, (== en)) |-> AT%:en
  ]

-- TODO advanced voiceleading \w configuration
voiceLead :: Music SemiChord -> IO (Music Chord)
voiceLead = return . fmap (\pcs -> (\p -> (p, def)) <$> pcs)

type ListMusic a = [(a, Duration)]
type ListMusicM a = [(Maybe a, Duration)]
toListM :: Music a -> ListMusicM a
toListM (m :+: m') = toListM m ++ toListM m'
toListM (_ :=: _)  = error "toList: non-sequential music"
toListM (Note d a) = [(Just a, d)]
toListM (Rest d)   = [(Nothing, d)]

toList :: Music a -> ListMusic a
toList (m :+: m') = toList m ++ toList m'
toList(Note d a)  = [(a, d)]
toList (_ :=: _)  = error "toListFull: non-sequential music"
toList (Rest _)   = error "toListFull: rest exists"

fromListM :: ListMusicM a -> Music a
fromListM ((Just a,t):ms)  = a <| t :+: fromListM ms
fromListM ((Nothing,t):ms) = (t~~) :+: fromListM ms
fromListM []               = (0~~)

mkSolo :: Music SemiChord -> Music NT -> IO Melody
mkSolo chs nts = fromListM <$> go (toList chs) (toList nts)
  where
    go :: ListMusic SemiChord -> ListMusic NT -> IO (ListMusicM Pitch)
    go [] _ = return []
    go _ [] = return []
    go ((ch, t):back) front = do
      let (ps', front') = takeTime front t
      (++) <$> forM ps' (interpretNT ch)
           <*> go back front'

    mkPitch :: Duration -> [PitchClass] -> IO (Maybe Pitch, Duration)
    mkPitch t ps = do
      p <- oneOf ps
      oct <- choose [(1, Oct3), (10,Oct4), (10, Oct5), (1, Oct6)]
      return (Just $ p#oct, t)

    distancePc :: PitchClass -> PitchClass -> Interval
    distancePc pc pc' = distanceP (pc#oct) (pc#(oct + offset))
      where oct = Oct4
            offset | pc > pc'  = 1
                   | otherwise = 0

    distanceP :: Pitch -> Pitch -> Interval
    distanceP p p' = P8 - toEnum (fromEnum $ p - p')

    toIntervals :: SemiChord -> AbstractChord
    toIntervals ch = P1 : (uncurry distancePc <$> zip ch (tail ch))

    interpretNT :: SemiChord -> (NT, Duration) -> IO (Maybe Pitch, Duration)
    interpretNT ch (nt, t) =
      case nt of
        R -> return (Nothing, t)
        HT -> do
          nt' <- oneOf [CT, L, AT] -- TODO weights
          interpretNT ch (nt', t)
        CT -> mkPitch t ch
        AT -> mkPitch t (((~> Mi2) <$> ch) ++ ((<~ Mi2) <$> ch))
        ST -> do
          let scales = [ sc
                       | sc <- allScales
                       , all (`elem` sc) (toIntervals ch)
                       ]
          if null scales then
            interpretNT ch (CT, t)
          else do
            sc <- oneOf scales
            mkPitch t (head ch +| sc)
        L -> interpretNT ch (ST, t)
        _  -> error $ "intrepret: incomplete grammar rewrite " ++ show nt ++ " <| " ++ show t

    takeTime :: ListMusic NT -> Duration -> (ListMusic NT, ListMusic NT)
    takeTime ntz d
      | d <= 0 = ([], ntz)
      | otherwise = case ntz of
          [] -> ([], [])
          (nt@(_, d'):ntz') ->
            let (ntz'', rest) = takeTime ntz' (d - d')
            in  (nt:ntz'', rest)

final :: PitchClass -> Duration -> IO MusicCore
final pc t = do
  harmonicStructure <- runGrammar harmony (I, t) pc
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=: toMusicCore foreground
  where
    soften (p, _) = p <: [Dynamic PP]

-- TODO weights for `allChords`, `mkSolo`
