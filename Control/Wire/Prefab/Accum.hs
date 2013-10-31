-- |
-- Module:     Control.Wire.Prefab.Accum
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Accumulation wires.  These are left-scan equivalents of several
-- sorts.

module Control.Wire.Prefab.Accum
    ( -- * General
      -- ** Accumulation
      accum,
      accumT,
      accum1,
      accumT1,
      -- ** Function iteration
      iterateW,
      iterateWT,
      -- ** Generic unfolding
      unfold,
      unfoldT,

       -- * Special
      countFrom,
      enumFromW,
      mconcatW
    )
    where

import Control.Wire.Wire
import Data.AdditiveGroup
import Data.Monoid
import Prelude hiding (enumFrom, iterate)


-- | The most general accumulator.  This wire corresponds to a left
-- scan.
--
-- * Depends: previous instant.

accum :: (b -> a -> b) -> b -> Wire e m a b
accum f = accumT (const f)


-- | Non-delaying variant of 'accum'.
--
-- * Depends: current instant.

accum1 :: (b -> a -> b) -> b -> Wire e m a b
accum1 f = accumT1 (const f)


-- | Like 'accum', but the accumulation function also receives the
-- current time delta.
--
-- * Depends: previous instant.

accumT :: (Time -> b -> a -> b) -> b -> Wire e m a b
accumT f x' =
    mkPure $ \dt x ->
        x' `seq` (Right x', accumT f (f dt x' x))


-- | Non-delaying variant of 'accumT'.
--
-- * Depends: current instant.

accumT1 :: (Time -> b -> a -> b) -> b -> Wire e m a b
accumT1 f x' =
    mkPure $ \dt x ->
        let y = f dt x' x in
        x' `seq` (Right y, accumT1 f y)


-- | Counts from the given vector adding the current input for the next
-- instant.
--
-- * Depends: previous instant.

countFrom :: (AdditiveGroup b) => b -> Wire e m b b
countFrom = accum (^+^)


-- | Enumerates from the given element.

enumFromW :: (Enum b) => b -> Wire e m a b
enumFromW = accum (\x _ -> succ x)


-- | Apply the input function continously.  Corresponds to 'iterate' for
-- lists.

iterateW :: (b -> b) -> b -> Wire e m a b
iterateW f = accum (\x _ -> f x)


-- | Like 'iterate', but the accumulation function also receives the
-- current time delta.

iterateWT :: (Time -> b -> b) -> b -> Wire e m a b
iterateWT f = accumT (\dt x _ -> f dt x)


-- | Running 'Monoid' sum.
--
-- * Depends: previous instant.

mconcatW :: (Monoid b) => Wire e m b b
mconcatW = accum mappend mempty


-- | Corresponds to 'unfoldr' for lists.
--
-- * Depends: current instant, if the unfolding function is strict in
-- its second argument.

unfold :: (s -> a -> (b, s)) -> s -> Wire e m a b
unfold = unfoldT . const


-- | Like 'unfold', but the accumulation function also receives the
-- current time delta.
--
-- * Depends: current instant, if the given function is strict in its
-- third argument.

unfoldT :: (Time -> s -> a -> (b, s)) -> s -> Wire e m a b
unfoldT f s' =
    mkPure $ \dt x' ->
        let (x, s) = f dt s' x' in
        s' `seq` (Right x, unfoldT f s)
