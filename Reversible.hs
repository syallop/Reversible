{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs, KindSignatures, TypeOperators, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-|
Module      : Reversible
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

A description of reversible functions.

One use case is for dual parser and printer functions which should be each
others inverse. Pairing these functions into a 'reversible' abstraction can
prevent round-trip properties from being accidentally violated as definitions
are changed.
-}
module Reversible
  ( ReversibleInstr (..)

  , rpure
  , rempty
  , ralt
  , rmap
  , rap

  , (\|/)
  , (\*/)
  , (\$/)
  , (*/)
  , (\*)

  , between
  , betweenMany
  , betweenMany1
  , alternatives
  , rmany
  , rmany1
  , allowed
  , required
  , prefered

  , Reversible (..)
  , reversible
  )
  where

import Prelude hiding ((.),id)

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Foldable
import Data.Monoid
import Reversible.Iso

import DSL.Instruction

-- | A 'ReversibleInstr' is a computation with some result type 'a' supporting:
-- - Injecting of pure values via 'RPure'.
-- - Applying reversible 'Iso' functions via 'RMap'.
-- - Alternatives and failure via 'REmpty' and 'RAlt'.
-- - Sequential application via 'RAp'.
-- - You may inject your own instruction type 'i'.
data ReversibleInstr (i :: (* -> *) -> * -> *) (p :: * -> *) (a :: *) where
  -- | RPure is similar to 'Applicative' 'pure' - it reversibles a value typed 'a'
  -- into the 'Reversible' context.
  -- Unlike Applicative, we must be able to compare the value for equality to
  -- permit reversal.
  RPure
    :: Eq a
    => a
    -> ReversibleInstr i p a

  -- | REmpty is similar to 'Alternative's 'empty' - it encodes a notion of
  -- failure.
  REmpty
    :: ReversibleInstr i p a

  -- | RAlt is similar to 'Alternative's '<|>' - it encodes a notion of choice.
  RAlt
    :: p a
    -> p a
    -> ReversibleInstr i p a

  -- | RMap is similar to 'Functor's 'fmap' - it allows mapping a function over
  -- something in the 'Reversible' context.
  -- Unlike fmap which takes a plain function, RMap accepts an 'Iso' to permit
  -- reversal.
  RMap
    :: Show a
    => Iso a b
    -> p a
    -> ReversibleInstr i p b

  -- | RAp is similar to 'Applicative's '<*>' or 'Monad's 'ap' - it encodes a
  -- notion of sequential application.
  -- Unlike '<*>', RAp tuples its sequential results instead of using a plain
  -- function to permit reversal.
  RAp
    :: (Show a, Show b)
    => p a
    -> p b
    -> ReversibleInstr i p (a,b)

  ReversibleInstr
    :: i p a
    -> ReversibleInstr i p a

newtype Reversible i a = Reversible (ReversibleInstr i (Reversible i) a)

-- | Assert an instruction type is reversible.
reversible
  :: i :<- i'
  => i (Reversible i') a
  -> Reversible i' a
reversible = Reversible . ReversibleInstr . inj

-- | rpure is similar to 'Applicative' 'pure' - it reversibles a value typed 'a'
-- into the 'Reversible' context.
-- Unlike Applicative, we must be able to compare the value for equality to
-- permit reversal.
rpure
  :: Eq a
  => a
  -> Reversible i a
rpure = Reversible . RPure

-- | rempty is similar to 'Alternative's 'empty' - it encodes a notion of
-- failure.
rempty
  :: Reversible i a
rempty = Reversible REmpty

-- | ralt is similar to 'Alternative's '<|>' - it encodes a notion of choice.
ralt
  :: Reversible i a
  -> Reversible i a
  -> Reversible i a
ralt l r = Reversible $ RAlt l r

-- | rmap is similar to 'Functor's 'fmap' - it allows mapping a function over
-- something in the 'Reversible' context.
-- Unlike fmap which takes a plain function, rmap accepts an 'Iso' to permit
-- reversal.
rmap
  :: Show a
  => Iso a b
  -> Reversible i a
  -> Reversible i b
rmap iso p = Reversible $ RMap iso p

-- | rap is similar to 'Applicative's '<*>' or 'Monad's 'ap' - it encodes a
-- notion of sequential application.
-- Unlike '<*>', rap tuples its sequential results instead of using a plain
-- function to permit reversal.
rap
  :: ( Show a
     , Show b
     )
  => Reversible i a
  -> Reversible i b
  -> Reversible i (a,b)
rap l r = Reversible $ RAp l r

infixl 3 \|/
infixr 6 \*/
infix  5 \$/

-- | Infix 'ralt'.
(\|/)
  :: Reversible i a
  -> Reversible i a
  -> Reversible i a
(\|/) = ralt

-- | Infix 'rap'.
(\*/)
  :: ( Show a
     , Show b
     )
  => Reversible i a
  -> Reversible i b
  -> Reversible i (a,b)
(\*/) = rap

-- | Infix 'rmap'.
(\$/)
  :: ( Show a
     , Show b
     )
  => Iso a b
  -> Reversible i a
  -> Reversible i b
(\$/) = rmap

-- | Execute a 'Reversible' with a discardable result, then sequentially execute
-- another.
-- Similar to 'Applicative's *>' except the discarded computation must return
-- '()'.
(*/)
  :: Show a
  => Reversible i ()
  -> Reversible i a
  -> Reversible i a
rl */ rr = inverse unitIso . flipIso \$/ rl \*/ rr

-- | Execute a 'Reversible' with a result to return, then sequentially execute
-- another with a discardable result.
-- Similar to 'Applicative's <*' except the discarded computation must return
-- '()'.
(\*)
  :: Show a
  => Reversible i a
  -> Reversible i ()
  -> Reversible i a
rl \* rr = inverse unitIso \$/ rl \*/ rr

-- | A 'Reversible' function between two others.
between
  :: Show a
  => Reversible i ()
  -> Reversible i a
  -> Reversible i ()
  -> Reversible i a
between l a r = l */ a \* r

-- | A 'Reversible' function which may occur between many matching pairs of some
-- other 'Reversible's.
--
-- E.G.
--
-- betweenMany "(" r ")" is either:
-- - r
-- - (r)
-- - ((r))
-- but never:
-- - (r
-- - ((r)
-- - (((r
-- as the between reversibles do not match.
betweenMany
  :: Show a
  => Reversible i ()
  -> Reversible i a
  -> Reversible i ()
  -> Reversible i a
betweenMany l a r =
  a \|/ (l */ (between l a r) \* r)

-- | 'betweenMany' where the 'reversible' must occur with atleast one matching
-- pair of some other 'Reversibles'.
betweenMany1
  :: Show a
  => Reversible i ()
  -> Reversible i a
  -> Reversible i ()
  -> Reversible i a
betweenMany1 l a r = l */ betweenMany l a r \* r

-- | A list of alternative Reversibles.
alternatives
  :: [Reversible i a]
  -> Reversible i a
alternatives = foldr ralt rempty

-- | Zero or many reversible functions.
rmany
  :: ( Eq a
     , Show a
     )
  => Reversible i a
  -> Reversible i [a]
rmany g = rmany1 g \|/ rpure []

-- | One or many reversible functions.
rmany1
  :: ( Eq a
     , Show a
     )
  => Reversible i a
  -> Reversible i [a]
rmany1 g = consIso \$/ g \*/ rmany g

-- | A reversible computation is allowed to succeed when ran forwards but
-- ignored backwards.
allowed
  :: Reversible i ()
  -> Reversible i ()
allowed g = ignoreIso [] \$/ rmany g

-- | A reversible computation is required to succeed when ran forwards and runs
-- backwards.
required
  :: Reversible i ()
  -> Reversible i ()
required g = g \* allowed g

-- | A reversible computation is allowed to succeed when ran forwards, and runs
-- backwards.
prefered
  :: Reversible i ()
  -> Reversible i ()
prefered g = ignoreIso [()] \$/ rmany g

