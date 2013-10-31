-- |
-- Module:     Control.Wire.Prefab.Move
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- This module provides the wires for various kinds of moving objects.
-- In particular this includes various calculus wires like integrals and
-- differentials.

module Control.Wire.Prefab.Move
    ( -- * Calculus
      -- ** Integrals
      integral,
      integral_,
      integralLim,
      integralLim_,
      integral1,
      integral1_,
      integralLim1,
      integralLim1_,
      -- ** Differentials
      derivative,
      derivative_,

      -- * Simulations/games
      object,
      object_,
      ObjectState(..),
      ObjectDiff(..)
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Wire.Prefab.Accum
import Control.Wire.Prefab.Time
import Control.Wire.Wire
import Data.Data
import Data.VectorSpace
import Prelude hiding ((.), id)


-- | Object state.  This includes the position and velocity.

data ObjectState a =
    ObjectState {
      objPosition :: a,  -- ^ Position.
      objVelocity :: a   -- ^ Velocity.
    }
    deriving (Data, Eq, Ord, Read, Show, Typeable)


-- | Differential for objects.

data ObjectDiff a
    -- | Accelerate (units per second).
    = Accelerate a

    -- | Teleport to the given position instantly (velocity will be
    -- unchanged).
    | Position a

    -- | Specify velocity (units per second).
    | Velocity a
    deriving (Data, Eq, Ord, Read, Show, Typeable)


-- | Derivative.  Receives @x@ and @dt@ and calculates the change rate
-- @dx/dt@.  Note that @dt@ despite its name does not have to be time.
--
-- The exception handler function is called when @dt@ is zero.  That
-- function's result is the wire's output for those instants.  If you
-- don't want to handle exceptional cases specially, just pass @(^/)@ as
-- the handler function.
--
-- * Depends: current instant.

derivative ::
    (Eq dt, Fractional dt, VectorSpace b, Scalar b ~ dt)
    => (b -> dt -> b)  -- ^ Handle exceptional change rates (receives dx and dt).
    -> b               -- ^ Initial position.
    -> Wire e m (b, dt) b
derivative catch x0 =
    mkPure $ \_ (x1, dt) ->
        let dx = x1 ^-^ x0
            d | dt == 0    = catch dx dt
              | otherwise = dx ^/ dt
        in (Right d, derivative catch x1)


-- | Same as 'derivative', but with respect to time.
--
-- * Depends: current instant.

derivative_ ::
    (Monad m, VectorSpace b, Scalar b ~ Time)
    => (b -> Time -> b)  -- ^ Handle exceptional cases.
    -> b                 -- ^ Initial position.
    -> Wire e m b b
derivative_ catch x0 = derivative catch x0 . (id &&& dtime)


-- | Integral wire.  Produces position from velocity in the sense of the
-- given vector space.
--
-- * Depends: previous instant.

integral ::
    (VectorSpace b)
    => b
    -> Wire e m (b, Scalar b) b
integral = accum (\x (dx, dt) -> x ^+^ dt *^ dx)


-- | Non-delaying variant of 'integral'.
--
-- * Depends: current instant.

integral1 ::
    (VectorSpace b)
    => b
    -> Wire e m (b, Scalar b) b
integral1 = accum1 (\x (dx, dt) -> x ^+^ dt *^ dx)


-- | Same as 'integral', but with respect to time.
--
-- * Depends: previous instant.

integral_ ::
    (VectorSpace b, Scalar b ~ Time)
    => b
    -> Wire e m b b
integral_ = accumT (\dt x dx -> x ^+^ dt *^ dx)


-- | Non-delaying variant of 'integral_'.
--
-- * Depends: current instant.

integral1_ ::
    (Monad m, VectorSpace b, Scalar b ~ Time)
    => b
    -> Wire e m b b
integral1_ = accumT1 (\dt x dx -> x ^+^ dt *^ dx)


-- | Variant of 'integral', where you can specify a post-update
-- function, which receives the previous position as well as the current
-- (in that order).  This is useful for limiting the output (think of
-- robot arms that can't be moved freely).
--
-- * Depends: current instant if the post-update function is strict in
-- its first argument, previous instant if not.

integralLim ::
    (VectorSpace b)
    => (w -> b -> b -> b)  -- ^ Post-update function.
    -> b                   -- ^ Initial value.
    -> Wire e m ((b, w), Scalar b) b
integralLim uf = accum (\x ((dx, w), dt) -> uf w x (x ^+^ dt *^ dx))


-- | Non-delaying variant of 'integralLim'.
--
-- * Depends: current instant.

integralLim1 ::
    (VectorSpace b)
    => (w -> b -> b -> b)  -- ^ Post-update function.
    -> b                   -- ^ Initial value.
    -> Wire e m ((b, w), Scalar b) b
integralLim1 uf = accum1 (\x ((dx, w), dt) -> uf w x (x ^+^ dt *^ dx))


-- | Same as 'integralLim', but with respect to time.
--
-- * Depends: previous instant.

integralLim_ ::
    (VectorSpace b, Scalar b ~ Time)
    => (w -> b -> b -> b)
    -> b
    -> Wire e m (b, w) b
integralLim_ uf = accumT (\dt x (dx, w) -> uf w x (x ^+^ dt *^ dx))


-- | Non-delaying variant of 'integralLim_'.
--
-- * Depends: current instant.

integralLim1_ ::
    (VectorSpace b, Scalar b ~ Time)
    => (w -> b -> b -> b)
    -> b
    -> Wire e m (b, w) b
integralLim1_ uf = accumT1 (\dt x (dx, w) -> uf w x (x ^+^ dt *^ dx))


-- | Objects are generalized integrals.  They are controlled through
-- velocity and/or acceleration and can be collision-checked as well as
-- instantly teleported.
--
-- The post-move update function receives the world state and the
-- current object state.  It is applied just before the wire produces
-- its output.  You can use it to perform collision-checks or to limit
-- the velocity.
--
-- Note that teleportation doesn't change the velocity.
--
-- * Depends: current instant.

object ::
    forall b m dt e w.
    (VectorSpace b, Scalar b ~ dt)
    => (w -> ObjectState b -> ObjectState b)  -- ^ Post-move update function.
    -> ObjectState b                          -- ^ Initial state.
    -> Wire e m (ObjectDiff b, w, dt) (ObjectState b)
object uf = loop
    where
    applyDiff :: dt -> ObjectDiff b -> ObjectState b -> ObjectState b
    applyDiff dt (Accelerate dv) (ObjectState x' v') = ObjectState x v
        where
        v = v' ^+^ dt *^ dv
        x = x' ^+^ dt *^ v
    applyDiff _  (Position x) (ObjectState _ v)  = ObjectState x v
    applyDiff dt (Velocity v) (ObjectState x' _) = ObjectState (x' ^+^ dt *^ v) v

    loop :: ObjectState b -> Wire e m (ObjectDiff b, w, dt) (ObjectState b)
    loop os' =
        mkPure $ \_ (dos, w, dt) ->
            let os = uf w . applyDiff dt dos $ os'
            in (Right os, loop os)


-- | Same as 'object', but with respect to time.
--
-- * Depends: current instant.

object_ ::
    (Monad m, VectorSpace b, Scalar b ~ Time)
    => (w -> ObjectState b -> ObjectState b)  -- ^ Post-move update function.
    -> ObjectState b                          -- ^ Initial state.
    -> Wire e m (ObjectDiff b, w) (ObjectState b)
object_ uf x0 = object uf x0 . liftA2 (\(dx, w) dt -> (dx, w, dt)) id dtime
