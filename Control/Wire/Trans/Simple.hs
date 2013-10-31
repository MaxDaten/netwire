-- |
-- Module:     Control.Wire.Trans.Simple
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Basic wire combinators.

module Control.Wire.Trans.Simple
    ( -- * Predicate-based
      ifW
    )
    where

import Control.Arrow
import Control.Monad
import Control.Wire.Wire
import Data.Monoid
import Prelude hiding ((.), id)


-- | The wire @ifW p x y@ acts like @x@, when the predicate @p@ is true,
-- otherwise @y@.
--
-- * Complexity: like the predicate and the chosen wire.
--
-- * Depends: like the predicate and the chosen wire.
--
-- * Inhibits: when the predicate or the chosen wire inhibits.

ifW ::
    (Monad m, Monoid e)
    => Wire e m a Bool  -- ^ Predicate.
    -> Wire e m a b     -- ^ If true.
    -> Wire e m a b     -- ^ If false.
    -> Wire e m a b
ifW = ifW' 0 0
    where
    ifW' !tx !ty wp' wx' wy' =
        mkGen $ \dt x' -> do
            (mb, wp) <- stepWire wp' dt x'
            case mb of
              Left ex -> return (Left ex, ifW' (tx + dt) (ty + dt) wp wx' wy')
              Right b ->
                  if b
                    then liftM (second (\wx -> ifW' 0 (ty + dt) wp wx wy')) $
                         stepWire wx' (tx + dt) x'
                    else liftM (second (ifW' (tx + dt) 0 wp wx')) $
                         stepWire wy' (ty + dt) x'
