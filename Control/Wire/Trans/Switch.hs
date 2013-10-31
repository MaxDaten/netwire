-- |
-- Module:     Control.Wire.Trans.Switch
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Switching combinators.  Notice that these combinators restart time
-- when switching.

module Control.Wire.Trans.Switch
    ( -- * Simple switching
      andThen,
      switch,
      switchBy,
      (-->)
    )
    where

import Control.Arrow
import Control.Monad
import Control.Wire.Wire


-- | Infix variant of 'andThen'.
--
-- This operator is right-associative with precedence 1.

(-->) :: (Monad m) => Wire e m a b -> Wire e m a b -> Wire e m a b
(-->) = andThen

infixr 1 -->


-- | Behaves like the first wire until it inhibits.  Switches to the
-- second wire as soon as the first one inhibits.
--
-- The @`andThen`@ operator is right-associative with precedence 1.
--
-- * Depends: like currently active wire.
--
-- * Inhibits: when switched to second wire and that one inhibits.
--
-- * Time: switching restarts time.

andThen ::
    (Monad m)
    => Wire e m a b  -- ^ Wire to start with.
    -> Wire e m a b  -- ^ Wire to switch into.
    -> Wire e m a b
andThen w1' w2' =
    mkGen $ \dt x' -> do
        (mx, w1) <- stepWire w1' dt x'
        case mx of
          Left _  -> stepWire w2' dt x'
          Right _ -> return (mx, andThen w1 w2')

infixr 1 `andThen`


-- | If the first argument wire produces a wire, switch to it
-- immediately.  If not, evolve the current wire.  The second argument
-- wire is the initial wire.
--
-- * Depends: like event wire and the currently active wire.
--
-- * Inhibits: when the currently active wire inhibits.
--
-- * Time: switching restarts time.

switch ::
    (Monad m)
    => Wire e m a (Wire e m a b)  -- ^ Produces a wire to switch into.
    -> Wire e m a b               -- ^ Initial wire.
    -> Wire e m a b
switch wnew' w0 =
    mkGen $ \dt x' -> do
        (w', wnew) <- liftM (first (either (const w0) id)) (stepWire wnew' dt x')
        (mx, w) <- stepWire w' dt x'
        return (mx, switch wnew w)


-- | Whenever the given wire inhibits, a new wire is constructed using
-- the given function.
--
-- * Depends: like currently active wire.
--
-- * Time: switching restarts time.

switchBy ::
    (Monad m)
    => (e' -> Wire e' m a b)  -- ^ Wire selection function.
    -> Wire e' m a b          -- ^ Initial wire.
    -> Wire e m a b
switchBy new w0 =
    mkGen $ \dt x' ->
        let select w' = do
                (mx, w) <- stepWire w' dt x'
                case mx of
                  Left ex -> select (new ex)
                  Right x -> return (Right x, switchBy new w)
        in select w0
