-- |
-- Module:     Control.Wire.Prefab.List
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Wires from lists.

module Control.Wire.Prefab.List
    ( -- * Wires from lists
      cycleW,
      list
    )
    where

import Control.Applicative
import Control.Monad.Fix
import Control.Wire.Wire
import Data.Monoid


-- | Produce the values in the given list cycling forever.
--
-- * Inhibits: when the argument list is empty.

cycleW :: (Monad m, Monoid e) => [b] -> Wire e m a b
cycleW [] = empty
cycleW xs = fix (\again -> foldr cons again xs)


-- | Produce the values in the given list and then inhibit forever.
--
-- * Inhibits: when the list is exhausted.

list :: (Monad m, Monoid e) => [b] -> Wire e m a b
list = foldr cons empty
