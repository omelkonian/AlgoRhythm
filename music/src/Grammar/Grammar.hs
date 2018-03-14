{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Grammar.Grammar
       ( Weight
       , Grammar, Rule (..), Head, Activation, Body, Terminal
       , Term (..), Expand (..), Grammarly
       , runGrammar, always
       , (-|), ($:), (|$:), (|->), (%:)
       ) where

import System.Random

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
data Term a meta = Prim (Terminal a)           -- primitive
                 | Term a meta :-: Term a meta -- sequence
                 | Aux Bool meta (Term a meta) -- auxiliary modifications
                 deriving (Eq, Show)
            -- TODO add let + var
            -- | Let x (a -> Term)   -- let (enables repetition)
            -- | Var a              -- variable placeholders

type Grammarly input a meta b = (Eq a, Eq meta, Expand input a meta b)

class Expand input a meta b | input a meta -> b where
  -- | Expand meta-information.
  expand :: input -> Term a meta -> IO (Term b ())

  -- | Convert to music (after expansion).
  toMusic :: input -> Term a meta -> IO (Music b)
  toMusic input term = do
    expanded <- expand input term
    go expanded
    where go (Prim (a, t)) = return $ Note t a
          go (t :-: t')    = (:+:) <$> toMusic () t <*> toMusic () t'
          go (Aux _ () t)  = toMusic () t

-- | A term with no auxiliaries can be trivially expanded.
instance Expand input a () a where
  expand = const return

-- | Run a grammar with the given initial symbol.
runGrammar :: Grammarly input a meta b
           => Grammar a meta -> Terminal a -> input -> IO (Music b)
runGrammar grammar initial input = do
  applyRules <- fixpoint (go grammar) (Prim initial)
  toMusic input applyRules
  where
    -- | Run one term of grammar rewriting.
    go :: (Eq meta, Eq a) => Grammar a meta -> Term a meta -> IO (Term a meta)
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

-- | Set a primitive term's duration.
(%:) :: a -> Duration -> Term a meta
m %: t = Prim (m, t)

-- | Rule with duration-independent body.
(|->) :: Head a -> Term a meta -> Rule a meta
a |-> b = a :-> const b

-- | Identity rule.
(-|) :: a -> Weight -> Rule a meta
a -| w = (a, w, always) :-> \t -> Prim (a, t)

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
