-- |
-- Module:     Control.Wire.Trans.Combine
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Wire combinators to manage sets of wires.

module Control.Wire.Trans.Combine
    ( -- * Multiplexing
      context,
      contextLatest,
      contextLimit,

      -- * Multicast
      multicast
    )
    where

import qualified Control.Wire.TimedMap as Tm
import qualified Data.Map as M
import qualified Data.Traversable as T
import Control.Wire.TimedMap (TimedMap)
import Control.Wire.Wire
import Data.Map (Map)


-- | The argument function turns the input signal into a context.  For
-- each context the given base wire evolves individually.
--
-- Note: Incorrect usage can lead to a memory leak.  Consider using
-- 'contextLimit' instead.
--
-- * Complexity:  O(n) space, O(log n) time wrt to number of stored
-- contexts.
--
-- * Depends: current instant.
--
-- * Inhibits: when the context wire inhibits.

context ::
    forall a b m e k. (Monad m, Ord k)
    => (a -> k)      -- ^ Function to turn the signal into a context.
    -> Wire e m a b  -- ^ Base wire.
    -> Wire e m a b
context key w0 = context' M.empty 0
    where
    context' :: Map k (Wire e m a b, Time) -> Time -> Wire e m a b
    context' !ctxs t' =
        mkGen $ \dt' x' -> do
            let ctx      = key x'
                (w', t0) = M.findWithDefault (w0, t') ctx ctxs
                t        = t' + dt'
                dt       = t - t0
            (mx, w) <- dt `seq` stepWire w' dt x'
            return (mx, context' (M.insert ctx (w, t) ctxs) t)


-- | Same as 'context', but keeps only the latest given number of
-- contexts.

contextLatest ::
    (Monad m, Ord k)
    => (a -> k)      -- ^ Signal to context.
    -> Int           -- ^ Maximum number of latest wires.
    -> Wire e m a b  -- ^ Base wire.
    -> Wire e m a b
contextLatest key maxWires = contextLimit key (\_ _ -> Tm.cut maxWires)


-- | Same as 'context', but applies the given cleanup function to the
-- context map at every instant.  This can be used to drop older wires.

contextLimit ::
    forall a b m e k. (Monad m, Ord k)
    => (a -> k)      -- ^ Function to turn the signal into a context.
    -> (forall w. Int -> Time -> TimedMap Time k w -> TimedMap Time k w)
            -- ^ Cleanup function.  Receives the current instant number,
            -- the current local time and the current map.
    -> Wire e m a b  -- ^ Base wire.
    -> Wire e m a b
contextLimit key uf w0 = context' 0 Tm.empty 0
    where
    context' :: Int -> TimedMap Time k (Wire e m a b) -> Time -> Wire e m a b
    context' !n !ctxs t' =
        mkGen $ \dt' x' -> do
            let ctx      = key x'
                (w', t0) = Tm.findWithDefault (w0, t') ctx ctxs
                t        = t' + dt'
                dt       = t - t0
            (mx, w) <- dt `seq` stepWire w' dt x'
            return (mx, context' (n + 1) (uf n t (Tm.insert t ctx w ctxs)) t)


-- | Broadcast the input signal to all of the given wires collecting
-- their results.  Each of the given subwires is evolved individually.
--
-- * Depends: like the most dependent subwire.
--
-- * Inhibits: when any of the subwires inhibits.

multicast ::
    (Monad m, T.Traversable f)
    => f (Wire e m a b)
    -> Wire e m a (f b)
multicast ws' =
    mkGen $ \dt x' -> do
        res <- T.mapM (\w -> stepWire w dt x') ws'
        let resx = T.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res
        return (fmap (fmap fst) resx, multicast (fmap snd res))
