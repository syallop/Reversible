{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
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
  ( Reversible (..)

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
  , alternatives
  , rmany
  , rmany1
  , allowed
  , required
  , prefered
  )
  where

import Prelude hiding ((.),id)

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Foldable
import Data.Monoid
import Reversible.Iso

-- | A 'Reversible' is a computation with some result type 'a' supporting:
-- - Injecting of pure values via 'RPure'.
-- - Applying reversible 'Iso' functions via 'RMap'.
-- - Alternatives and failure via 'REmpty' and 'RAlt'.
-- - Sequential application via 'RAp'.
data Reversible a where
  -- | RPure is similar to 'Applicative' 'pure' - it injects a value typed 'a'
  -- into the 'Reversible' context.
  -- Unlike Applicative, we must be able to compare the value for equality to
  -- permit reversal.
  RPure
    :: Eq a
    => a
    -> Reversible a

  -- | REmpty is similar to 'Alternative's 'empty' - it encodes a notion of
  -- failure.
  REmpty
    :: Reversible a

  -- | RAlt is similar to 'Alternative's '<|>' - it encodes a notion of choice.
  RAlt
    :: Reversible a
    -> Reversible a
    -> Reversible a

  -- | RMap is similar to 'Functor's 'fmap' - it allows mapping a function over
  -- something in the 'Reversible' context.
  -- Unlike fmap which takes a plain function, RMap accepts an 'Iso' to permit
  -- reversal.
  RMap
    :: Show a
    => Iso a b
    -> Reversible a
    -> Reversible b

  -- | RAp is similar to 'Applicative's '<*>' or 'Monad's 'ap' - it encodes a
  -- notion of sequential application.
  -- Unlike '<*>', RAp tuples its sequential results instead of using a plain
  -- function to permit reversal.
  RAp
    :: (Show a, Show b)
    => Reversible a
    -> Reversible b
    -> Reversible (a,b)

-- | rpure is similar to 'Applicative' 'pure' - it injects a value typed 'a'
-- into the 'Reversible' context.
-- Unlike Applicative, we must be able to compare the value for equality to
-- permit reversal.
rpure
  :: Eq a
  => a
  -> Reversible a
rpure = RPure

-- | rempty is similar to 'Alternative's 'empty' - it encodes a notion of
-- failure.
rempty
  :: Reversible a
rempty = REmpty

-- | ralt is similar to 'Alternative's '<|>' - it encodes a notion of choice.
ralt
  :: Reversible a
  -> Reversible a
  -> Reversible a
ralt = RAlt

-- | rmap is similar to 'Functor's 'fmap' - it allows mapping a function over
-- something in the 'Reversible' context.
-- Unlike fmap which takes a plain function, rmap accepts an 'Iso' to permit
-- reversal.
rmap
  :: Show a
  => Iso a b
  -> Reversible a
  -> Reversible b
rmap = RMap

-- | rap is similar to 'Applicative's '<*>' or 'Monad's 'ap' - it encodes a
-- notion of sequential application.
-- Unlike '<*>', rap tuples its sequential results instead of using a plain
-- function to permit reversal.
rap
  :: ( Show a
     , Show b
     )
  => Reversible a
  -> Reversible b
  -> Reversible (a,b)
rap = RAp

infixl 3 \|/
infixr 6 \*/
infix  5 \$/

-- | Infix 'ralt'.
(\|/)
  :: Reversible a
  -> Reversible a
  -> Reversible a
(\|/) = ralt

-- | Infix 'rap'.
(\*/)
  :: ( Show a
     , Show b
     )
  => Reversible a
  -> Reversible b
  -> Reversible (a,b)
(\*/) = rap

-- | Infix 'rmap'.
(\$/)
  :: ( Show a
     , Show b
     )
  => Iso a b
  -> Reversible a
  -> Reversible b
(\$/) = rmap

-- | Execute a 'Reversible' with a discardable result, then sequentially execute
-- another.
-- Similar to 'Applicative's *>' except the discarded computation must return
-- '()'.
(*/)
  :: Show a
  => Reversible ()
  -> Reversible a
  -> Reversible a
rl */ rr = inverse unitIso . flipIso \$/ rl \*/ rr

-- | Execute a 'Reversible' with a result to return, then sequentially execute
-- another with a discardable result.
-- Similar to 'Applicative's <*' except the discarded computation must return
-- '()'.
(\*)
  :: Show a
  => Reversible a
  -> Reversible ()
  -> Reversible a
rl \* rr = inverse unitIso \$/ rl \*/ rr

-- | A 'Reversible' function between two others.
between
  :: Show a
  => Reversible ()
  -> Reversible a
  -> Reversible ()
  -> Reversible a
between l a r = l */ a \* r

-- | A list of alternative Reversibles.
alternatives
  :: [Reversible a]
  -> Reversible a
alternatives = foldr ralt rempty

-- | Zero or many reversible functions.
rmany
  :: ( Eq a
     , Show a
     )
  => Reversible a
  -> Reversible [a]
rmany g = rmany1 g \|/ rpure []

-- | One or many reversible functions.
rmany1
  :: ( Eq a
     , Show a
     )
  => Reversible a
  -> Reversible [a]
rmany1 g = consIso \$/ g \*/ rmany g

-- | A reversible computation is allowed to succeed when ran forwards but
-- ignored backwards.
allowed
  :: Reversible ()
  -> Reversible ()
allowed g = ignoreIso [] \$/ rmany g

-- | A reversible computation is required to succeed when ran forwards and runs
-- backwards.
required
  :: Reversible ()
  -> Reversible ()
required g = g \* allowed g

-- | A reversible computation is allowed to succeed when ran forwards, and runs
-- backwards.
prefered
  :: Reversible ()
  -> Reversible ()
prefered g = ignoreIso [()] \$/ rmany g

