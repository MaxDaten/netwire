-- |
-- Module:     Control.Wire.Prefab.Time
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Time wires.

module Control.Wire.Prefab.Time
    ( -- * Time
      dtime,
      time,
      timeFrom
    )
    where

import Control.Wire.Wire


-- | Outputs the time delta to the last instant.
--
-- * Depends: time.

dtime :: Wire e m a Time
dtime = mkFix (\dt _ -> Right dt)


-- | Outputs the current local time passed since the first instant.
--
-- * Depends: time.

time :: Wire e m a Time
time = timeFrom 0


-- | Outputs the current local time passed since the first instant with
-- the given offset.
--
-- * Depends: time.

timeFrom :: Time -> Wire e m a Time
timeFrom t' =
    mkPure $ \dt _ ->
        let t = t' + dt in
        t `seq` (Right t, timeFrom t)
