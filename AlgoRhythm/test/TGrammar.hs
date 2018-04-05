{-# LANGUAGE MultiParamTypeClasses #-}
module TGrammar where

import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     ((@?=))

import Grammar
import Music

data LK = L | K deriving (Eq, Show)

data Flip = Flip deriving (Eq, Show)

lkGrammar :: Grammar Flip LK
lkGrammar = L |:
  [ (L, 1, (== wn)) :-> \t -> Let (K:%:t/4 :-: L:%:t/4) (\x -> x :-: x)
  , (L, 1, (== qn)) :-> \t -> Flip |$: (L:%:t/2 :-: K:%:t/2)
  ]

instance Expand () LK Flip LK where
  expand () (m :-: m')              = (:-:) <$> expand () m <*> expand () m'
  expand () (Aux _ Flip (m :-: m')) = (:-:) <$> expand () m' <*> expand () m
  expand () (Aux _ _ m)             = expand () m
  expand () (x :%: d)               = return $ x :%: d
  expand _ _                        = error "Expand: let-expressions exist"

grammarTests :: Test
grammarTests = testGroup "Grammar"
  [ testCase "LK-grammar" $ do
      fin <- runGrammar lkGrammar wn ()
      fin @?= ((K<|qn :+: (K<|en :+: L<|en)) :+: (K<|qn :+: (K<|en :+: L<|en)))
  ]
