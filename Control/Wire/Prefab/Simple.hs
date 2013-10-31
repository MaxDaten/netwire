-- |
-- Module:     Control.Wire.Prefab.Simple
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Basic wires.

module Control.Wire.Prefab.Simple
    ( -- * Basic signal manipulation
      append,
      delay,
      prepend,

      -- * Forcing evaluation
      force,
      forceNF
    )
    where

import Control.Arrow
import Control.Category
import Control.DeepSeq (NFData, ($!!))
import Control.Wire.Wire
import Prelude hiding ((.), id)


-- | Convenience function to add another signal.
--
-- * Depends: current instant.

append :: (Monad m) => Wire e m a b -> Wire e m a (a, b)
append = (id &&&)


-- | One-instant delay.
--
-- * Depends: previous instant.

delay :: a -> Wire e m a a
delay x' = mkPure (\_ x -> x' `seq` (Right x', delay x))


-- | Acts like the identity wire, but forces evaluation of the signal to
-- WHNF.
--
-- * Depends: current instant.

force :: Wire e m a a
force = mkFix (\_ -> (Right $!))


-- | Acts like the identity wire, but forces evaluation of the signal to
-- NF.
--
-- * Depends: current instant.

forceNF :: (NFData a) => Wire e m a a
forceNF = mkFix (\_ -> (Right $!!))


-- | Convenience function to add another signal.
--
-- * Depends: current instant.

prepend :: (Monad m) => Wire e m a b -> Wire e m a (b, a)
prepend = (&&& id)
