-- | Generating rules for "Language.Ebnf.Extensions".
{-# LANGUAGE RecursiveDo #-}

module Language.Ebnf.Extensions.Earley
  ( mkOptRule
  , mkRep0Rule
  , mkRep1Rule
  , mkRepsep0Rule
  , mkRepsep1Rule
  ) where

import Control.Applicative (asum)
import qualified Data.List.NonEmpty as NE
import Language.Ebnf.Extensions.Syntax
  ( Opt
  , Rep0
  , Rep1
  , Repsep0(..)
  , Repsep1(..)
  )
import Text.Earley (Grammar, Prod, rule)

-- | Generate a rule to parse an 'Opt'.
mkOptRule :: Prod r e t b -> Grammar r (Prod r e t (Opt b))
mkOptRule body = rule $ asum [Just <$> body, pure Nothing]

-- | Generate a rule to parse a 'Rep0'.
mkRep0Rule :: Prod r e t b -> Grammar r (Prod r e t (Rep0 b))
mkRep0Rule body =
  mdo rep0 <- rule $ asum [(:) <$> body <*> rep0, pure []]
      pure rep0

-- | Generate a rule to parse a 'Rep1'.
mkRep1Rule :: Prod r e t b -> Grammar r (Prod r e t (Rep1 b))
mkRep1Rule body =
  mdo rep1 <- rule $ asum [NE.cons <$> body <*> rep1, NE.singleton <$> body]
      pure rep1

-- | Generate a rule to parse a 'Repsep0'.
mkRepsep0Rule ::
     Prod r e t s -> Prod r e t b -> Grammar r (Prod r e t (Repsep0 s b))
mkRepsep0Rule sep body =
  mdo repsep1 <- mkRepsep1Rule sep body
      repsep0 <- rule $ asum [Repsep0Just <$> repsep1, pure Repsep0Nothing]
      pure repsep0

-- | Generate a rule to parse a 'Repsep1'.
mkRepsep1Rule ::
     Prod r e t s -> Prod r e t b -> Grammar r (Prod r e t (Repsep1 s b))
mkRepsep1Rule sep body =
  mdo repsep1 <-
        rule $
        asum
          [Repsep1Cons <$> body <*> sep <*> repsep1, Repsep1Singleton <$> body]
      pure repsep1
