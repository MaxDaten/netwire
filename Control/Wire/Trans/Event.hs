-- |
-- Module:     Control.Wire.Trans.Event
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Event-related wire combinators.

module Control.Wire.Trans.Event
    ( -- * Combinators
      eitherE,
      (<||>),

      -- * Holding events
      hold,
      hold_,
      holdFor,
      holdForI,

      -- * Inhibition
      (<!>),
      event,
      exhibit,
      gotEvent,
      notE
    )
    where

import Control.Arrow
import Control.Category
import Control.Wire.Types
import Control.Wire.Wire
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Prelude hiding ((.), id)


-- | Try both wires combining their results with the given functions.
--
-- * Like argument wires.
--
-- * Inhibits: when both wires inhibit.

eitherE ::
    (Monad m, Monoid e)
    => (b1 -> b)        -- ^ Only left.
    -> (b2 -> b)        -- ^ Only right.
    -> (b1 -> b2 -> b)  -- ^ Both.
    -> Wire e m a b1   -- ^ First wire.
    -> Wire e m a b2   -- ^ Second wire.
    -> Wire e m a b
eitherE left right both = eitherE'
    where
    eitherE' w1' w2' =
        mkGen $ \dt x' -> do
            (mx1, w1) <- stepWire w1' dt x'
            (mx2, w2) <- stepWire w2' dt x'
            let res =
                    case (mx1, mx2) of
                      (Left ex1, Left ex2) -> Left (mappend ex1 ex2)
                      (Right x1, Right x2) -> Right (both x1 x2)
                      (Right x1, _)        -> Right (left x1)
                      (_, Right x2)        -> Right (right x2)
            return (res, eitherE' w1 w2)


-- | Semigroup version of 'eitherE'.

(<||>) ::
    (Monad m, Monoid e, Semigroup b)
    => Wire e m a b
    -> Wire e m a b
    -> Wire e m a b
(<||>) = eitherE id id (<>)


-- | If the argument wire inhibits, inhibit with the given exception
-- instead.
--
-- * Depends: like argument wire.
--
-- * Inhibits: like argument wire.

(<!>) :: (Monad m) => Wire e m a b -> e -> Wire e m a b
w <!> ex = mapOutput (either (Left . const ex) Right) w


-- | Prevent a wire from inhibiting.  Instead produce a signal wrapped
-- in 'Maybe'.
--
-- Note:  You probably shouldn't use this function.
--
-- * Depends: like argument wire.

event :: (Monad m) => Wire e m a b -> Wire e m a (Maybe b)
event = mapOutput (Right . either (const Nothing) Just)


-- | Prevent a wire from inhibiting.  Instead produce the inhibition
-- value.
--
-- Note:  You probably shouldn't use this function.
--
-- * Depends: like argument wire.

exhibit :: (Monad m) => Wire e m a b -> Wire e m a (Either e b)
exhibit = mapOutput Right


-- | Prevent a wire from inhibiting.  Instead produce 'False', if the
-- wire inhibited.
--
-- Note:  You probably shouldn't use this function.
--
-- * Depends: like argument wire.

gotEvent :: (Monad m) => Wire e m a b -> Wire e m a Bool
gotEvent = mapOutput (Right . either (const False) (const True))


-- | Hold the latest event.  Produces the last produced value starting
-- with the given one.
--
-- * Depends: like argument wire.

hold :: (Monad m) => b -> Wire e m a b -> Wire e m a b
hold x0 w' =
    mkGen $ \dt x' -> do
        (mx, w) <- stepWire w' dt x'
        case mx of
          Left _  -> return (Right x0, hold x0 w)
          Right x -> return (Right x, hold x w)


-- | Hold the event.  Once the argument wire produces the produced value
-- is held until the argument wire produces again.
--
-- * Depends: like argument wire.
--
-- * Inhibits: until the argument wire produces for the first time.

hold_ :: (Monad m) => Wire e m a b -> Wire e m a b
hold_ w' =
    mkGen $ \dt x' -> do
        (mx, w) <- stepWire w' dt x'
        return (mx, either (const hold_) hold mx w)


-- | Hold the event for the given amount of time.  When the argument
-- wire produces, the produced value is kept for the given amount of
-- time.  If the wire produces again while another value is kept, the
-- new value takes precedence.
--
-- * Depends: like argument wire.
--
-- * Inhibits: as described.

holdFor :: (Monad m) => Time -> Wire e m a b -> Wire e m a b
holdFor t0 w = hold' . exhibit w
    where
    hold' =
        mkPure $ \_ mx ->
            case mx of
              Left _  -> (mx, hold')
              Right x -> (mx, hold'' t0 x)

    hold'' t' x' =
        mkPure $ \dt mx ->
            let t = t' - dt in
            case mx of
              Left _
                  | t > 0     -> (Right x', hold'' t x')
                  | otherwise -> (mx, hold')
              Right x -> (mx, hold'' t0 x)


-- | Hold the event for the given number of instances.  When the
-- argument wire produces, the produced value is kept for the given
-- number of instances.  If the wire produces again while another value
-- is kept, the new value takes precedence.
--
-- * Depends: like argument wire.
--
-- * Inhibits: as described.

holdForI :: (Monad m) => Int -> Wire e m a b -> Wire e m a b
holdForI t0 w = hold' . exhibit w
    where
    hold' = mkPure $ \_ -> id &&& either (const hold') (hold'' t0)

    hold'' t x'
        | t <= 0 = hold'
        | otherwise =
            mkPure $ \_ mx ->
                case mx of
                  Left _  -> (Right x', hold'' (t - 1) x')
                  Right x -> (mx, hold'' t0 x)


-- | Act like the identity wire, if the argument wire inhibits.
-- Inhibit, if the argument wire produces.
--
-- * Depends: like argument wire.
--
-- * Inhibits: when argument wire produces.

notE :: (Monad m, Monoid e) => Event e m a -> Event e m a
notE w' =
    mkGen $ \dt x' -> do
        (mx, w) <- stepWire w' dt x'
        return (either (const $ Right x') (const $ Left mempty) mx, notE w)
