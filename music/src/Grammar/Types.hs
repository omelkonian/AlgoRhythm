{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
module Grammar.Types
       ( Weight
       , Grammar, Rule (..), Head, Activation, Body, Terminal
       , Term (..), Expand (..), Grammarly
       , runGrammar, always, (/\), (\/)
       , (-|), (-||), ($:), (|$:), (|->), (%:)
       ) where

import System.Random
import Text.Show.Functions ()

import Generate (Weight)
import Music

{- Operators' precedence. -}
infix 6 %:
infix 5 $:
infix 5 |$:
infixr 4 :-:
infix 3 :->
infix 3 |->

{- Grammar datatypes. -}
type Grammar a meta = [Rule a meta]
data Rule a meta = Head a :-> Body a meta
type Head a = (a, Weight, Activation)
type Activation = Duration -> Bool
type Body a meta = Duration -> Term a meta
type Terminal a = (a, Duration)

data Term a meta = -- primitive
                   Prim (Terminal a)
                   -- sequence
                   | Term a meta :-: Term a meta
                   -- auxiliary modifications
                   | Aux Bool meta (Term a meta)
                   -- let (enables repetition)
                   | Let (Term a meta) (forall b. Term b () -> Term b ())

deriving instance (Show a, Show meta) => Show (Term a meta)

instance (Eq a, Eq meta) => Eq (Term a meta) where
  (Prim (a, d))  == (Prim (a', d'))   = a == a' && d == d'
  (x :-: y)      == (x' :-: y')       = x == x' && y == y'
  (Aux b meta t) == (Aux b' meta' t') = b == b' && meta == meta' && t == t'
  (Let t _)      == (Let t' _)        = t == t'
  _              == _                 = False

type Grammarly input a meta b =
  (Show a, Show meta, Eq a, Eq meta, Expand input a meta b)

-- | Any metadata-carrying grammar term must be expanded to a stripped-down
-- grammar term with no metadata (i.e. `Term a ()`), possibly producing terms of
-- a different type `b`.
class Expand input a meta b | input a meta -> b where
  -- | Expand meta-information.
  expand :: input -> Term a meta -> IO (Term b ())

-- | Convert to music (after expansion).
toMusic :: (Expand input a meta b) => input -> Term a meta -> IO (Music b)
toMusic input term = do
  expanded <- expand input term
  go $ unlet expanded
  where go (Prim (a, t)) = return $ Note t a
        go (t :-: t')    = (:+:) <$> toMusic () t <*> toMusic () t'
        go _             = error "toMusic: lets/aux after expansion"

        unlet (Let x f)    = unlet (f x)
        unlet (t :-: t')   = unlet t :-: unlet t'
        unlet (Aux _ () t) = unlet t
        unlet t            = t

-- | A term with no auxiliaries can be trivially expanded.
instance Expand input a () a where
  expand = const return

-- | Run a grammar with the given initial symbol.
runGrammar :: Grammarly input a meta b
           => Grammar a meta -> Terminal a -> input -> IO (Music b)
runGrammar grammar initial input = do
  rewritten <- fixpoint (go grammar) (Prim initial)
  toMusic input rewritten
  where
    -- | Run one term of grammar rewriting.
    go :: (Eq meta, Eq a) => Grammar a meta -> Term a meta -> IO (Term a meta)
    -- go _ (Var x) = return $ Var x
    go gram (Let x f) = do
      x' <- go gram x
      return $ Let x' f
    go gram (t :-: t') =
      (:-:) <$> go gram t <*> go gram t'
    go _ a@(Aux True _ _) =
      return a
    go gram (Aux False meta term) =
      Aux False meta <$> go gram term
    go gram (Prim term@(a, t)) = do
      let rules = filter (\((a', _, activ) :-> _) -> a' == a && activ t) gram
      (_ :-> rewrite) <- pickRule term rules
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

-- | Set a primitive term's duration.
(%:) :: a -> Duration -> Term a meta
m %: t = Prim (m, t)

-- | Rule with duration-independent body.
(|->) :: Head a -> Term a meta -> Rule a meta
a |-> b = a :-> const b

-- | Identity rule.
(-|) :: a -> Weight -> Rule a meta
a -| w = (a, w, always) :-> \t -> Prim (a, t)

-- | Identity rule with activation function.
(-||) :: (a, Weight) -> Activation -> Rule a meta
(a, w) -|| f = (a, w, f) :-> \t -> Prim (a, t)

-- | Operators for auxiliary terms.
($:), (|$:) :: meta -> Term a meta -> Term a meta
($:) = Aux False -- auxiliary symbol that allows internal rewriting
(|$:) = Aux True -- frozen auxiliary symbol

{- Helpers. -}

-- | Randomly pick a rule to rewrite given terminal.
pickRule :: Terminal a -> Grammar a meta -> IO (Rule a meta)
pickRule (a, _) [] = return $ a -| 1
pickRule _ rs = do
  let totalWeight = sum ((\((_, w, _) :-> _) -> w) <$> rs)
  index <- getStdRandom $ randomR (0, totalWeight)
  return $ pick' index rs
  where pick' :: Double -> Grammar a meta -> Rule a meta
        pick' n (r@((_, w, _) :-> _):rest) =
          if n <= w then r else pick' (n-w) rest
        pick' _ _ = error "pick: empty list"

-- | Converge to fixpoint with given initial value.
fixpoint :: Eq a => (a -> IO a) -> a -> IO a
fixpoint k l = do
  l' <- k l
  if l == l' then return l else fixpoint k l'
