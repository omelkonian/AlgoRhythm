{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
module Grammar.Types
       ( Weight
       , Grammar (..), Rule (..), Head, Activation, Body
       , Term (..), Expand (..), Grammarly
       , runGrammar, always, (/\), (\/)
       , (|:), (-|), (-||), ($:), (|$:), (|->)
       ) where

import System.Random
import Text.Show.Functions ()

import Generate (Weight)
import Music

{- Operators' precedence. -}
infix 6 :%:
infix 5 $:
infix 5 |$:
infixr 4 :-:
infix 3 :->
infix 3 |->

{- Grammar datatypes. -}
data Grammar meta a = Grammar { initial :: a, rules :: [Rule meta a] }
infix 2 |:
(|:) :: a -> [Rule meta a] -> Grammar meta a
initA |: rs = Grammar initA rs

data Rule meta a = Head a :-> Body meta a
type Head a = (a, Weight, Activation)
type Activation = Duration -> Bool
type Body meta a = Duration -> Term meta a
-- type Terminal a = (a, Duration)

data Term meta a = -- primitive
                   a :%: Duration
                   -- sequence
                   | Term meta a :-: Term meta a
                   -- auxiliary modifications
                   | Aux Bool meta (Term meta a)
                   -- let (enables repetition)
                   | Let (Term meta a) (Term meta a -> Term meta a)

deriving instance (Show a, Show meta) => Show (Term meta a)

instance (Eq a, Eq meta) => Eq (Term meta a) where
  (a :%: d)      == (a' :%: d')       = a == a' && d == d'
  (x :-: y)      == (x' :-: y')       = x == x' && y == y'
  (Aux b meta t) == (Aux b' meta' t') = b == b' && meta == meta' && t == t'
  (Let t _)      == (Let t' _)        = t == t'
  _              == _                 = False

instance Functor (Term meta) where
  fmap f m = case m of
    a :%: t            -> f a :%: t
    m1 :-: m2          -> (f <$> m1) :-: (f <$> m2)
    Aux frozen meta m1 -> Aux frozen meta (f <$> m1)
    _                  -> error "fmap: let-expressions exist"

type Grammarly input a meta b =
  (Show a, Show meta, Eq a, Eq meta, Expand input a meta b)

-- | Any metadata-carrying grammar term must be expanded to a stripped-down
-- grammar term with no metadata (i.e. `Term a ()`), possibly producing terms of
-- a different type `b`.
class Expand input a meta b | input a meta -> b where
  -- | Expand meta-information.
  expand :: input -> Term meta a -> IO (Term () b)

-- | Convert to music (after expansion).
toMusic :: (Expand input a meta b) => input -> Term meta a -> IO (Music b)
toMusic input term = do
  expanded <- expand input (unlet term)
  go expanded
  where go (a :%: t)  = return $ Note t a
        go (t :-: t') = (:+:) <$> toMusic () t <*> toMusic () t'
        go _          = error "toMusic: lets/aux after expansion"

        unlet (Let x k)      = unlet (k x)
        unlet (t :-: t')     = unlet t :-: unlet t'
        unlet (Aux b meta t) = Aux b meta (unlet t)
        unlet t              = t

-- | A term with no auxiliary wrappers can be trivially expanded.
instance Expand input a () a where
  expand = const return

-- | Run a grammar with the given initial symbol.
runGrammar :: Grammarly input a meta b
           => Grammar meta a -> Duration -> input -> IO (Music b)
runGrammar grammar initT input = do
  rewritten <- fixpoint (go grammar) (initial grammar :%: initT)
  toMusic input rewritten
  where
    -- | Run one term of grammar rewriting.
    go :: (Eq meta, Eq a) => Grammar meta a -> Term meta a -> IO (Term meta a)
    -- go _ (Var x) = return $ Var x
    go gram (Let x k) = do
      x' <- go gram x
      return $ Let x' k
    go gram (t :-: t') =
      (:-:) <$> go gram t <*> go gram t'
    go _ a@(Aux True _ _) =
      return a
    go gram (Aux False meta term) =
      Aux False meta <$> go gram term
    go (Grammar _ rs) (a :%: t) = do
      let rs' = filter (\((a', _, activ) :-> _) -> a' == a && activ t) rs
      (_ :-> rewrite) <- pickRule a rs'
      return $ rewrite t

{- Grammar-specific operators. -}

-- | Rule which always activates.
always :: Activation
always = const True

-- | Conjunction of activation functions.
(/\) :: Activation -> Activation -> Activation
(f /\ g) x = f x && g x

-- | Disjunction of activation functions.
(\/) :: Activation -> Activation -> Activation
(f \/ g) x = f x || g x

-- | Rule with duration-independent body.
(|->) :: Head a -> Term meta a -> Rule meta a
a |-> b = a :-> const b

-- | Identity rule.
(-|) :: a -> Weight -> Rule meta a
a -| w = (a, w, always) :-> \t -> a :%: t

-- | Identity rule with activation function.
(-||) :: (a, Weight) -> Activation -> Rule meta a
(a, w) -|| f = (a, w, f) :-> \t -> a :%: t

-- | Operators for auxiliary terms.
($:), (|$:) :: meta -> Term meta a -> Term meta a
($:) = Aux False -- auxiliary symbol that allows internal rewriting
(|$:) = Aux True -- frozen auxiliary symbol

{- Helpers. -}

-- | Randomly pick a rule to rewrite given terminal.
pickRule :: a -> [Rule meta a] -> IO (Rule meta a)
pickRule a [] = return $ a -| 1
pickRule _ rs = do
  let totalWeight = sum ((\((_, w, _) :-> _) -> w) <$> rs)
  index <- getStdRandom $ randomR (0, totalWeight)
  return $ pick' index rs
  where pick' :: Double -> [Rule meta a] -> Rule meta a
        pick' n (r@((_, w, _) :-> _):rest) =
          if n <= w then r else pick' (n-w) rest
        pick' _ _ = error "pick: empty list"

-- | Converge to fixpoint with given initial value.
fixpoint :: Eq a => (a -> IO a) -> a -> IO a
fixpoint k l = do
  l' <- k l
  if l == l' then return l else fixpoint k l'
