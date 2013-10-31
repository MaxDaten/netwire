-- |
-- Module:     Control.Wire.Prefab.Noise
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Various noise generators.

module Control.Wire.Prefab.Noise
    ( -- * Pure random noise
      noise,
      noiseR,
      wackelkontakt,

      -- * Effectful random noise
      noiseM,
      noiseRM,
      wackelkontaktM
    )
    where

import Control.Monad
import Control.Wire.Classes
import Control.Wire.Prefab.Accum
import Control.Wire.Types
import Control.Wire.Wire
import Data.Monoid
import System.Random


-- | Pure noise generator.

noise ::
    (Random b, RandomGen g)
    => g  -- ^ Initial random number generator.
    -> Wire e m a b
noise = unfold (\g' _ -> random g')


-- | Noise generator.

noiseM ::
    (MonadRandom m, Random b)
    => Wire e m a b
noiseM =
    mkFixM $ \_ _ -> liftM (Right $!) getRandom


-- | Ranged noise generator.
--
-- * Depends: current instant.

noiseRM ::
    (MonadRandom m, Random b)
    => Wire e m (b, b) b
noiseRM = mkFixM $ \_ -> liftM (Right $!) . getRandomR


-- | Pure ranged noise generator.
--
-- * Depends: current instant.

noiseR ::
    (Random b, RandomGen g)
    => g  -- ^ Initial random number generator.
    -> Wire e m (b, b) b
noiseR = unfold (\g' r -> randomR r g')


-- | Event:  Occurs randomly with the given probability.
--
-- * Inhibits: @wackelkontaktM p@ inhibits with probability @1 - p@.

wackelkontakt ::
    (Monoid e, RandomGen g)
    => Double  -- ^ Occurrence probability.
    -> g  -- ^ Initial random number generator.
    -> Event e m a
wackelkontakt p g' =
    mkPure $ \_ x ->
        let (e, g) = random g' in
        (if (e < p) then Right x else Left mempty, wackelkontakt p g)


-- | Event:  Occurs randomly with the given probability.
--
-- * Inhibits: @wackelkontaktM p@ inhibits with probability @1 - p@.

wackelkontaktM ::
    (MonadRandom m, Monoid e)
    => Double  -- ^ Occurrence probability.
    -> Event e m a
wackelkontaktM p =
    mkFixM $ \_ x -> do
        e <- getRandom
        return (if (e < p) then Right x else Left mempty)
