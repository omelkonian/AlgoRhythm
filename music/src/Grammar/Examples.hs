{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Grammar.Examples
       ( final
       , Config (..), defConfig
       ) where

import Control.Arrow (first)

import Grammar.Grammar
import Grammar.Utilities
import Grammar.VoiceLeading
import Music

---------------------------------- Harmony -------------------------------------
data Degree = I | II | III | IV | V | VI | VII
              deriving (Eq, Show, Enum, Bounded)

newtype Modulation = Modulation Interval deriving (Eq, Show)

harmony :: Grammar Degree Modulation
harmony =
  [ -- Turn-arounds
    (I, 8, (> wn)) :-> \t -> Let (I%:t/2) (\x -> x :-: x)
  , (I, 2, (> wn)) :-> \t -> I%:t/2 :-: I%:t/2
  , (I, 6, (> hn) /\ (<= wn)) :-> \t -> II%:t/4 :-: V%:t/4 :-: I%:t/2
  , (I, 2, (> hn) /\ (<= wn)) :-> \t -> V%:t/2 :-: I%:t/2
  , (I, 2) -|| (<= wn)
    -- TODO ++

    -- Modulations
  , (V, 5, (> hn)) :-> \t -> Modulation P5 $: I%:t
  , V -| 3
  -- , (II, 2, (> hn)) :-> \t -> Modulation M2 |$: I%:t
  -- , II -| 8
    -- TODO ++

    -- Tritone substitution
  -- , (V, 2, (> hn)) :-> \t -> Let (V%:t/4 :-: Modulation A4 |$: V%:t/4) (\x -> x :-: x)
    -- TODO ++
  ]

instance Expand Config Degree Modulation SemiChord where
  expand conf (m :-: m') = (:-:) <$> expand conf m <*> expand conf m'
  expand conf (Let x f)  = f <$> expand conf x
  expand conf (Aux _ (Modulation itv) t) =
    expand (conf {basePc = basePc conf ~~> itv}) t
  expand conf (Prim (a, t)) = do
    ch <- conf `interpret` a
    return $ Prim (ch, t)
    where
      interpret :: Config -> Degree -> IO SemiChord
      interpret config degree = choose options
        where tonic = basePc config +| baseScale config :: SemiScale
              tone = tonic !! fromEnum degree
              options = [ (w, ch)
                        | (w, chordType) <- chords config
                        , let ch = tone =| chordType
                        , all (`elem` tonic) ch
                        ]
---------------------------------- Melody --------------------------------------
data NT = MQ -- Meta-rhythm
        | Q  -- Rhythm non-terminal
        | MN -- Meta-note
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

  , (MN, 1, (== wn)) |-> MN%:qn :-: MN%:qn :-: MN%:qn :-: MN%:qn

  , (MN, 72, (== qn)) |-> MN%:en :-: MN%:en
  , (MN, 22, (== qn)) |-> N%:qn
  , (MN,  5, (== qn)) |-> HT%:(en^^^) :-: HT%:(en^^^) :-: HT%:(en^^^)
  , (MN,  1, (== qn)) |-> HT%:(en^^^) :-: HT%:(en^^^) :-: AT%:(en^^^)

  , (MN, 99, (== en)) |-> N%:en
  , (MN,  1, (== en)) |-> HT%:sn :-: AT%:sn

  , (N, 1, (== hn)) |-> CT%:hn

  , (N, 50, (== qn)) |-> CT%:qn
  , (N, 50, (== qn)) |-> ST%:qn
  -- , (N, 25, (== qn)) |-> R%:qn
  , (N, 45, (== qn)) |-> R%:qn
  , (N, 20, (== qn)) |-> L%:qn
  , (N,  1, (== qn)) |-> AT%:qn

  , (N, 40, (== en)) |-> CT%:en
  , (N, 40, (== en)) |-> ST%:en
  , (N, 20, (== en)) |-> L%:en
  -- , (N, 10, (== en)) |-> R%:en
  , (N, 20, (== en)) |-> R%:en
  , (N,  1, (== en)) |-> AT%:en
  ]

-- | Produce a concrete improvisation out of a melodic structure.
mkSolo :: (?config :: Config) => Music SemiChord -> Music NT -> IO Melody
mkSolo chs nts =
  fromListM <$> go Nothing [] (synchronize (toList chs) (toList nts))
  where
    go :: Maybe Pitch -> [Duration] -> ListMusic (SemiChord, NT) -> IO (ListMusicM Pitch)
    go _ _ [] = return []
    go prevP approach (((ch, nt), t):rest) =
      case nt of
        HT -> do
          nt' <- choose [(5, CT), (3, AT), (2, L)]
          go prevP approach (((ch, nt'), t):rest)
        AT -> if null rest then return [] else go prevP (approach ++ [t]) rest
        _  -> do m <- interpretNT prevP approach ch nt t
                 (++) <$> pure m <*> go (fst $ last m) [] rest

    interpretNT :: Maybe Pitch -- ^ previous pitch
                -> [Duration]  -- ^ approach tones
                -> SemiChord   -- ^ harmonic context
                -> NT          -- ^ current tone characteristic
                -> Duration    -- ^ current duration
                -> IO (ListMusicM Pitch)
    interpretNT prevP approach ch nt t =
      case nt of
        R -> return $ (,) Nothing <$> (t : approach)
        CT -> mkPitch prevP approach t ch
        ST ->
          let scales' = [(w, sc) | (w, sc) <- scales ?config, all (`elem` sc) (toIntervals ch)]
          in  if null scales'
              then interpretNT prevP approach ch CT t
              else do sc <- choose scales'
                      mkPitch prevP approach t (head ch +| sc)
        L -> let colors = colorTones ch
             in  if null colors
                 then interpretNT prevP approach ch CT t
                 else mkPitch prevP approach t colors
        _  -> error $ "intrepret: incomplete grammar rewrite " ++ show nt ++ " <| " ++ show t

    mkPitch :: Maybe Pitch -> [Duration] -> Duration -> [PitchClass] -> IO (ListMusicM Pitch)
    mkPitch prevP approach t pcs =
      let ps = [(pc#oct, w) | pc <- pcs, (w, oct) <- octaves ?config] :: [(Pitch, Weight)]
          setWeight (p', w') = w' * 1.0 / fromIntegral (pitchDistanceM prevP p')
      in  fst <$> chooseWith setWeight ps >>= approachPitch approach prevP t

    approachPitch :: [Duration] -> Maybe Pitch -> Duration -> Pitch -> IO (ListMusicM Pitch)
    approachPitch approach prevP t p = reverse <$> oneOf [move dir | dir <- directions]
      where
        move dir = first Just <$> zip (iterate (`dir` Mi2) p) (t : approach)
        directions = case prevP of
          Just p' -> if p' > p then [(<~)] else [(~>)]
          Nothing -> [(~>), (<~)]

    -- | Synchronize the harmonic background with the melodic foreground.
    synchronize :: ListMusic SemiChord -> ListMusic NT -> ListMusic (SemiChord, NT)
    synchronize [] _  = []
    synchronize _  [] = []
    synchronize ((ch, t):back) front =
      let (ps', front') = takeTime front t
      in  [((ch, p'), t') | (p', t') <- ps' ] ++ synchronize back front'

    takeTime :: ListMusic NT -> Duration -> (ListMusic NT, ListMusic NT)
    takeTime ntz d
      | d <= 0 = ([], ntz)
      | otherwise = case ntz of
          [] -> ([], [])
          (nt@(_, d'):ntz') ->
            let (ntz'', rest) = takeTime ntz' (d - d')
            in  (nt:ntz'', rest)

    -- | Extracts the color tones of a chord.
    colorTones :: SemiChord -> [PitchClass]
    colorTones (p:ps) = filter (\p' -> distancePc p p' `elem` colorIntervals) ps
      where colorIntervals = [M3, Mi3, Mi7, M7, Mi9, M9, M13, Mi13]
    colorTones [] = []

    toIntervals :: SemiChord -> AbstractChord
    toIntervals ch = P1 : (uncurry distancePc <$> zip ch (tail ch))

-------------------------------- Integration -----------------------------------

final :: (?config :: Config) => Duration -> IO MusicCore
final t = do
  let ?baseOctave = baseOct ?config
  harmonicStructure <- runGrammar harmony (I, t) ?config
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=:
           (soften' <$> toMusicCore foreground ~> P8)
  where
    soften (p, _) = p <: [Dynamic PPPP]
    soften' (p, _) = p <: [Dynamic P]

data Config = Config
  { baseOct   :: Octave
  , basePc    :: PitchClass
  , baseScale :: AbstractScale
  , chords    :: [(Weight, AbstractChord)]
  , scales    :: [(Weight, AbstractScale)]
  , octaves   :: [(Weight, Octave)]
  }

defConfig :: Config
defConfig = Config
  { baseOct = def
  , basePc  = def
  , baseScale = major
  , chords = equally allChords
  , scales = equally allScales
  , octaves = equally allOctaves
  }
