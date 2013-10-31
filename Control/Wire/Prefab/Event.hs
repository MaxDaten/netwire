-- |
-- Module:     Control.Wire.Prefab.Event
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Event wires.

module Control.Wire.Prefab.Event
    ( -- * Instants
      afterI,
      eventsI,
      forI,
      notYet,
      once,
      periodicallyI,

      -- * Signal analysis
      changed,
      inject,
      -- ** Predicate-based
      asSoonAs,
      edge,
      forbid,
      require,
      unless,
      until,
      when,
      while,

      -- * Time
      after,
      events,
      for,
      periodically,

      -- * Utilities
      inhibit
    )
    where

import Control.Category
import Control.Wire.Classes
import Control.Wire.Types
import Control.Wire.Wire
import Data.Monoid
import Prelude hiding ((.), id, until)


-- | Produce after the given amount of time.
--
-- * Depends: current instant when producing, time.
--
-- * Inhibits: until the given amount of time has passed.

after :: (Monoid e) => Time -> Event e m a
after t
    | t <= 0     = identity
    | otherwise = mkPure $ \dt _ -> (Left mempty, after (t - dt))


-- | Produce after the given number of instants.
--
-- * Depends: current instant when producing.
--
-- * Inhibits: until the given number of instants has passed.

afterI :: (Monoid e) => Int -> Event e m a
afterI t
    | t <= 0     = identity
    | otherwise = mkPure $ \_ _ -> (Left mempty, afterI (t - 1))


-- | Inhibit until the given predicate holds for the input signal.  Then
-- produce forever.
--
-- * Depends: current instant, if the predicate is strict.  Once true,
-- on current instant forever.
--
-- * Inhibits: until the predicate becomes true.

asSoonAs :: (Monoid e) => (a -> Bool) -> Event e m a
asSoonAs p =
    mkPure $ \_ x ->
        if p x
          then (Right x, identity)
          else (Left mempty, asSoonAs p)


-- | Produce when the signal has changed and at the first instant.
--
-- * Depends: current instant.
--
-- * Inhibits: after the first instant when the input has changed.

changed :: (Eq a, Monoid e) => Event e m a
changed = mkPure $ \_ x0 -> (Right x0, changed' x0)
    where
    changed' x' =
        mkPure $ \_ x ->
            (if x' == x then Left mempty else Right x,
             changed' x)


-- | Produces once whenever the given predicate switches from 'False' to
-- 'True'.
--
-- * Depends: current instant.
--
-- * Inhibits: when the predicate has not just switched from 'False' to
-- 'True'.

edge :: (Monoid e) => (a -> Bool) -> Event e m a
edge p = off
    where
    off = mkPure $ \_ x -> if p x then (Right x, on) else (Left mempty, off)
    on  = mkPure $ \_ x -> (Left mempty, if p x then on else off)


-- | Produce once periodically.  The production periods are given by the
-- argument list.  When it's @[1,2,3]@ it produces after one second,
-- then after two more seconds and finally after three more seconds.
-- When the list is exhausted, it never produces again.
--
-- * Depends: current instant when producing, time.
--
-- * Inhibits: between the given intervals.

events :: (Monoid e) => [Time] -> Event e m a
events [] = never
events (t':ts) =
    mkPure $ \dt x ->
        let t = t' - dt in
        if t <= 0
          then (Right x, events (mapHead (+ t) ts))
          else (Left mempty, events (t:ts))

    where
    mapHead :: (a -> a) -> [a] -> [a]
    mapHead _ []     = []
    mapHead f (x:xs) = f x : xs


-- | Variant of 'periodically' in number of instants instead of amount
-- of time.
--
-- * Depends: current instant when producing.
--
-- * Inhibits: between the given intervals.

eventsI :: (Monoid e) => [Int] -> Event e m a
eventsI [] = never
eventsI (0:ts) = mkPure $ \_ x -> (Right x, eventsI ts)
eventsI (t:ts) = mkPure $ \_ _ -> (Left mempty, eventsI (t - 1 : ts))


-- | Produce for the given amount of time.
--
-- * Depends: current instant when producing, time.
--
-- * Inhibits: after the given amount of time has passed.

for :: (Monoid e) => Time -> Event e m a
for t
    | t <= 0     = never
    | otherwise = mkPure $ \dt x -> (Right x, for (t - dt))


-- | Same as 'unless'.

forbid :: (Monoid e) => (a -> Bool) -> Event e m a
forbid = unless


-- | Produce for the given number of instants.
--
-- * Depends: current instant when producing.
--
-- * Inhibits: after the given number of instants has passed.

forI :: (Monoid e) => Int -> Event e m a
forI t
    | t <= 0     = never
    | otherwise = mkPure $ \_ x -> (Right x, forI (t - 1))


-- | Inhibit with the given value.  You may want to use a combination of
-- 'Control.Applicative.empty' and 'Control.Wire.Trans.Event.<!>'
-- instead.
--
-- * Inhibits: always.

inhibit :: e -> Wire e m a b
inhibit ex = mkFix (\_ _ -> Left ex)


-- | Inject the input signal.  Please keep in mind that in application
-- code it is almost always wrong to use this wire.  It should only be
-- used to interact with other frameworks/abstractions, and even then
-- it's probably just a last resort.
--
-- When you want to write your own wires, consider using 'mkPure' or the
-- various variants of it.
--
-- * Depends: current instant.
--
-- * Inhibits: depending on input signal (see 'Injectable').

inject :: (Injectable e f) => Wire e m (f b) b
inject = mkFix (const toSignal)


-- | Inhibit once.
--
-- * Depends: current instant after the first instant.
--
-- * Inhibits: in the first instant.

notYet :: (Monoid e) => Event e m a
notYet = mkPure $ \_ _ -> (Left mempty, identity)


-- | Produce once.
--
-- * Depends: current instant in the first instant.
--
-- * Inhibits: after the first instant.

once :: (Monoid e) => Event e m a
once = mkPure $ \_ x -> (Right x, never)


-- | Produce once periodically with the given time interval.
--
-- * Depends: current instant when producing, time.
--
-- * Inhibits: between the intervals.

periodically :: (Monoid e) => Time -> Event e m a
periodically = events . repeat


-- | Produce once periodically with the given number of instants as the
-- interval.
--
-- * Depends: current instant when producing.
--
-- * Inhibits: between the intervals.

periodicallyI :: (Monoid e) => Int -> Event e m a
periodicallyI = eventsI . repeat


-- | Same as 'when'.

require :: (Monoid e) => (a -> Bool) -> Event e m a
require = when


-- | Produce when the given predicate on the input signal does not hold.
--
-- * Depends: current instant if the predicate is strict.
--
-- * Inhibits: When the predicate is true.

unless :: (Monoid e) => (a -> Bool) -> Event e m a
unless p =
    mkFix $ \_ x ->
        if p x then Left mempty else Right x


-- | Produce until the given predicate on the input signal holds, then
-- inhibit forever.
--
-- * Depends: current instant, if the predicate is strict.
--
-- * Inhibits: forever as soon as the predicate becomes true.

until :: (Monoid e) => (a -> Bool) -> Event e m a
until p = while (not . p)


-- | Produce when the given predicate on the input signal holds.
--
-- * Depends: current instant if the predicate is strict.
--
-- * Inhibits: When the predicate is false.

when :: (Monoid e) => (a -> Bool) -> Event e m a
when p =
    mkFix $ \_ x ->
        if p x then Right x else Left mempty


-- | Produce while the given predicate on the input signal holds, then
-- inhibit forever.
--
-- * Depends: current instant, if the predicate is strict.
--
-- * Inhibits: forever as soon as the predicate becomes false.

while :: (Monoid e) => (a -> Bool) -> Event e m a
while p =
    mkPure $ \_ x ->
        if p x
          then (Right x, while p)
          else (Left mempty, never)
