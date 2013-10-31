-- |
-- Module:     Control.Wire.Prefab.Sample
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Signal sampling wires.

module Control.Wire.Prefab.Sample
    ( -- * Sampling
      --history,
      keep,
      sample,
      window,
      windowList
    )
    where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Control.Wire.Wire
import Data.Sequence (Seq, (|>))


-- | Produce the most recent inputs in the given time window.  The left
-- input signal is the sample, the right input signal is the time
-- window.
--
-- * Complexity: O(n), where n the number of samples in the time window.
--
-- * Depends: current instant.

--history :: (Reactive cat) => Wire e cat (a, Time) (Seq (a, Time))
--history = undefined


-- | Keep the input signal of the first instant forever.
--
-- Depends: first instant.

keep :: Wire e m a a
keep = mkPure (\_ x -> (Right x, constant x))


-- | Sample the left signal at discrete intervals given by the right
-- signal.
--
-- * Depends: instant of the last sample.

sample :: Wire e m (a, Time) a
sample = mkPure $ \dt (x, _) -> (Right x, sample' dt x)
    where
    sample' t0' x' =
        mkPure $ \dt (x, t1) ->
            let t0 = t0' + dt in
            if t0 >= t1
              then (Right x, sample' (t0 - t1) x)
              else (Right x', sample' t0 x')


-- | Produce up to the given number of most recent inputs.
--
-- * Complexity: O(n), where n is the given argument.
--
-- * Depends: current instant.

window :: Int -> Wire e m a (Seq a)
window = collect' Seq.empty
    where
    collect' s' 0 = window' s'
    collect' s' n =
        mkPure $ \_ x ->
            let s = s' |> x in
            s `seq` (Right s, collect' s (n - 1))

    window' s' =
        mkPure $ \_ x ->
            let s = Seq.drop 1 (s' |> x) in
            s `seq` (Right s, window' s)


-- | Same as @fmap toList . window@.

windowList :: (Monad m) => Int -> Wire e m a [a]
windowList = fmap F.toList . window
