-- |
-- Module:     Control.Wire.Prefab.Analyze
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Signal analysis wires.

module Control.Wire.Prefab.Analyze
    ( -- * Statistics
      -- ** Average
      avg,
      avgInt,
      avgAll,
      avgFps,
      avgFpsInt,
      -- ** Peak
      highPeak,
      lowPeak,
      peakBy,

      -- * Monitoring
      collect,
      firstSeen,
      lastSeen
    )
    where

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Control.Category
import Control.Wire.Prefab.Time
import Control.Wire.Wire
import Data.Map (Map)
import Data.Monoid
import Data.Sequence (Seq, ViewL(..), (|>), viewl)
import Data.VectorSpace
import Prelude hiding ((.), id)


-- | Calculate the average of the signal over the given number of last
-- samples.  If you need an average over all samples ever produced,
-- consider using 'avgAll' instead.
--
-- * Complexity: O(n) space wrt number of samples.
--
-- * Depends: current instant.

avg ::
    forall a m e v.
    (Fractional a, VectorSpace v, Scalar v ~ a)
    => Int
    -> Wire e m v v
avg n | n <= 0 = error "avg: The number of samples must be positive"
avg n =
    mkPure $ \_ x ->
        (Right x, avg' (Seq.replicate n (x ^/ d)) x)

    where
    avg' :: Seq v -> v -> Wire e m v v
    avg' samples'' a' =
        mkPure $ \_ x ->
            let xa              = x ^/ d
                xa' :< samples' = viewl samples''
                samples         = samples' |> xa
                a               = a' ^-^ xa' ^+^ xa
            in a `seq` (Right a, avg' samples a)

    d :: Scalar v
    d = realToFrac n


-- | Calculate the average of the input signal over all samples.  This
-- is usually not what you want.  In most cases the 'avg' wire is
-- preferable.
--
-- * Depends: current instant.

avgAll ::
    forall a m e v.
    (Fractional a, VectorSpace v, Scalar v ~ a)
    => Wire e m v v
avgAll = mkPure $ \_ x -> (Right x, avgAll' 1 x)
    where
    avgAll' :: a -> v -> Wire e m v v
    avgAll' n' a' =
        mkPure $ \_ x ->
            let n = n' + 1
                a = a' ^+^ (x ^-^ a') ^/ n
            in a' `seq` (Right a, avgAll' n a)


-- | Calculate the average number of instants per second for the last
-- given number of instants.  In a continuous game or simulation this
-- corresponds to the average number of frames per second, hence the
-- name.
--
-- * Complexity:  O(n) space wrt number of samples.
--
-- * Depends: time.

avgFps :: (Monad m) => Int -> Wire e m a Double
avgFps n = recip (avg n) . dtime


-- | Like 'avgFps', but sample in discrete intervals only.  This can
-- greatly enhance the performance, when you have an inefficient clock
-- source.
--
-- * Complexity:  O(n) space wrt number of samples.
--
-- * Depends: time.

avgFpsInt ::
    (Monad m)
    => Int  -- ^ Sampling interval.
    -> Int  -- ^ Number of samples.
    -> Wire e m a Double
avgFpsInt int n = recip (avgInt int n) . dtime


-- | Same as 'avg', but with a sampling interval.  This can be used to
-- increase the performance, if the input is complicated.
--
-- * Complexity: O(n) space wrt number of samples.
--
-- * Depends: current instant.

avgInt ::
    forall a m e v.
    (Fractional a, VectorSpace v, Scalar v ~ a)
    => Int  -- ^ Sampling interval.
    -> Int  -- ^ Number of samples.
    -> Wire e m v v
avgInt _ n | n <= 0 = error "avg: The number of samples must be positive"
avgInt int n =
    mkPure $ \_ x ->
        (Right x, avg' 0 (Seq.replicate n (x ^/ d)) x)

    where
    avg' :: Int -> Seq v -> v -> Wire e m v v
    avg' si samples'' a' | si < int = mkPure $ \_ _ -> (Right a', avg' (si + 1) samples'' a')
    avg' _ samples'' a' =
        mkPure $ \_ x ->
            let xa              = x ^/ d
                xa' :< samples' = viewl samples''
                samples         = samples' |> xa
                a               = a' ^-^ xa' ^+^ xa
            in a `seq` (Right a, avg' 0 samples a)

    d :: Scalar v
    d = realToFrac n


-- | Collect all distinct inputs ever received together with a count.
-- Elements not appearing in the map have not been observed yet.
--
-- * Complexity: O(n) space.
--
-- * Depends: current instant.

collect :: forall b m e. (Ord b) => Wire e m b (Map b Int)
collect = collect' M.empty
    where
    collect' :: Map b Int -> Wire e m b (Map b Int)
    collect' m' =
        mkPure $ \_ x ->
            let m = M.insertWith (+) x 1 m' in
            m `seq` (Right m, collect' m)


-- | Outputs the first local time the input was seen.
--
-- * Complexity: O(n) space, O(log n) time wrt number of samples so far.
--
-- * Depends: current instant, time.

firstSeen :: forall a m e. (Ord a) => Wire e m a Time
firstSeen = seen' 0 M.empty
    where
    seen' :: Time -> Map a Time -> Wire e m a Time
    seen' t' m' =
        mkPure $ \dt x ->
            let t = t' + dt in
            t `seq`
            case M.lookup x m' of
              Just xt -> (Right xt, seen' t m')
              Nothing ->
                  let m = M.insert x t m' in
                  m `seq` (Right t, seen' t m)


-- | High peak.
--
-- * Depends: current instant.

highPeak :: (Ord b) => Wire e m b b
highPeak = peakBy compare


-- | Outputs the local time the input was previously seen.
--
-- * Complexity: O(n) space, O(log n) time wrt number of samples so far.
--
-- * Depends: current instant, time.
--
-- * Inhibits: if this is the first time the input is seen.

lastSeen :: forall a m e. (Monoid e, Ord a) => Wire e m a Time
lastSeen = seen' 0 M.empty
    where
    seen' :: Time -> Map a Time -> Wire e m a Time
    seen' t' m' =
        mkPure $ \dt x ->
            let t = t' + dt
                m = M.insert x t m' in
            t `seq` m `seq`
            case M.lookup x m' of
              Just xt -> (Right xt, seen' t m)
              Nothing -> (Left mempty, seen' t m)


-- | Low peak.
--
-- * Depends: current instant.

lowPeak :: (Ord b) => Wire e m b b
lowPeak = peakBy (flip compare)


-- | Output the peak with respect to the given comparison function.
--
-- * Depends: current instant.

peakBy :: forall b m e. (b -> b -> Ordering) -> Wire e m b b
peakBy f = mkPure $ \_ x -> (Right x, peak' x)
    where
    peak' :: b -> Wire e m b b
    peak' x' =
        mkPure $ \_ x ->
            case f x' x of
              GT -> (Right x', peak' x')
              _  -> (Right x, peak' x)
