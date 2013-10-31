-- |
-- Module:     Control.Wire.Prefab.Queue
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Wires acting as queues.

module Control.Wire.Prefab.Queue
    ( -- * Queues
      bag,
      fifo,
      lifo
    )
    where

import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Control.Wire.Wire
import Data.Monoid
import Data.Set (Set)
import Data.Sequence (ViewL(..), (><), viewl)


-- | Incoming values are placed in a set, which is discharged element by
-- element.  Lower values are served first.  Duplicate values are served
-- once.
--
-- Note: Incorrect usage can lead to congestion.
--
-- * Complexity: O(n) space wrt bag size.
--
-- * Depends: current instant.
--
-- * Inhibits: when the bag is empty.

bag :: (Monoid e, Ord b) => Wire e m (Set b) b
bag = bag' S.empty
    where
    bag' s' =
        mkPure $ \_ xs ->
            case S.minView (S.union s' xs) of
              Nothing     -> (Left mempty, bag' S.empty)
              Just (x, s) -> (Right x, bag' s)


-- | First in, first out.  The input list is placed on the right end of
-- a queue at every instant, giving earlier elements a higher priority.
-- The queue is discharged item by item from the left.
--
-- Note: Incorrect usage can lead to congestion.
--
-- * Complexity: O(n) space wrt queue size.
--
-- * Depends: current instant.
--
-- * Inhibits: when the queue is currently empty.

fifo :: (Monoid e) => Wire e m [b] b
fifo = fifo' Seq.empty
    where
    fifo' queue' =
        mkPure $ \_ xs ->
            case viewl (queue' >< Seq.fromList xs) of
              EmptyL     -> (Left mempty, fifo' Seq.empty)
              x :< queue -> (Right x, fifo' queue)


-- | Last in, first out.  The input list is placed on a stack at every
-- instant, giving earlier elements a higher priority.  The stack is
-- discharged item by item from the top.
--
-- Note: Incorrect usage can lead to congestion.
--
-- * Complexity: O(n) space wrt stack size.
--
-- * Depends: current instant.
--
-- * Inhibits: when the stack is currently empty.

lifo :: (Monoid e) => Wire e m [b] b
lifo = lifo' Seq.empty
    where
    lifo' queue' =
        mkPure $ \_ xs ->
            case viewl (Seq.fromList xs >< queue') of
              EmptyL     -> (Left mempty, lifo' Seq.empty)
              x :< queue -> (Right x, lifo' queue)
