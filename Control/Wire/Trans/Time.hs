-- |
-- Module:     Control.Wire.Trans.Time
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Time-related wire combinators.

module Control.Wire.Trans.Time
    ( -- * Local time
      mapTime
    )
    where

import Control.Arrow
import Control.Monad
import Control.Wire.Wire


-- | Maps the given function over the time deltas for the given wire.
--
-- * Complexity: like argument wire.
--
-- * Depends: like argument wire.
--
-- * Inhibits: like argument wire.

mapTime :: (Monad m) => (Time -> Time) -> Wire e m a b -> Wire e m a b
mapTime f w' =
    mkGen $ \dt ->
        liftM (second (mapTime f)) . stepWire w' (f dt)
