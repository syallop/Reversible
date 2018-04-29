{-# LANGUAGE RankNTypes #-}
{-|
Module      : Reversible.Iso
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

Sometimes called 'Partial isomorphisms'. Objects which translate back and forth
between two types 'a' and 'b' with the possibility for failure.
Round-trips should not fail.
-}
module Reversible.Iso
  ( Iso (..)
  , forwards
  , backwards
  , inverse
  , compose

  , identityIso
  , nilIso
  , consIso
  , unitIso
  , flipIso
  , elementIso
  , ignoreIso
  )
  where

import Prelude hiding ((.),id)

import Control.Category
import Control.Monad
import Data.Monoid

-- | An Iso converts both ways between 'a' and 'b', each with an opportunity for
-- failure in each direction.
data Iso a b = Iso
  { _forwards  :: a -> Maybe b
  , _backwards :: b -> Maybe a
  }

-- | Iso's can be composed.
instance Category Iso where
  f . g = compose g f
  id = identityIso

-- | Apply an 'Iso' in the 'forwards' direction.
forwards
  :: Iso a b
  -> a
  -> Maybe b
forwards = _forwards

-- | Apply an 'Iso' in the 'backwards' direction.
backwards
  :: Iso a b
  -> b
  -> Maybe a
backwards = _backwards

-- | Reverse an 'Iso'.
inverse
  :: Iso a b
  -> Iso b a
inverse (Iso forwards backwards) = Iso backwards forwards

-- | Compose two 'Iso's.
compose
  :: Iso a b
  -> Iso b c
  -> Iso a c
compose f g = Iso (forwards f >=> forwards g) (backwards g >=> backwards f)

-- | The identity Iso maps something to itself, succeeding each time.
identityIso
  :: Iso a a
identityIso = Iso Just Just

{- Some example Isos -}

-- | The nil '[]' case of a list.
-- () <-> []
nilIso
  :: Iso () [a]
nilIso = Iso
  (\() -> Just [])
  (\xs -> case xs of
    []     -> Just ()
    (x:xs) -> Nothing
  )

-- | The cons ':' case of a list.
-- (x,xs) <-> (x:xs)
consIso
  :: Iso (a,[a]) [a]
consIso = Iso
  (\(x,xs) -> Just (x:xs))
  (\xs -> case xs of
    []     -> Nothing
    (x:xs) -> Just (x,xs)
  )

-- | The unit element for products.
-- a <-> (a,())
unitIso
  :: Iso a (a,())
unitIso = Iso
  (\a -> Just (a,()))
  (\(a,()) -> Just a)

-- | Products commute
-- (a,b) <-> (b,a)
flipIso
  :: Iso (a,b) (b,a)
flipIso = Iso
  (\(a,b) -> Just (b,a))
  (\(b,a) -> Just (a,b))

-- | Unit can be mapped between an element if we can judge elements equal.
-- () <-> a | a == e
elementIso
  :: Eq a
  => a
  -> Iso () a
elementIso a0 = Iso
  (const . Just $ a0)
  (\a1 -> if a0 == a1 then Just () else Nothing)

-- | Unit can be mapped between an element by ignoring what is applied.
-- () <-> a | a := e
ignoreIso
  :: a
  -> Iso a ()
ignoreIso a = Iso
  (const $ Just ())
  (const $ Just a)

