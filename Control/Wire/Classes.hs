-- |
-- Module:     Control.Wire.Classes
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Various type classes.

module Control.Wire.Classes
    ( -- * Effects
      MonadRandom(..),

      -- * Utility classes
      Injectable(..)
    )
    where

import Data.Monoid
import System.Random


-- | Class for injectable values.  See
-- 'Control.Wire.Prefab.Event.inject'.

class Injectable e f where
    toSignal :: f a -> Either e a

instance (Monoid e) => Injectable e Maybe where
    toSignal = maybe (Left mempty) Right

instance Injectable e (Either e) where
    toSignal = id


-- | Monads with a random number generator.

class (Monad m) => MonadRandom m where
    -- | Get a random number.
    getRandom :: (Random a) => m a

    -- | Get a random number in the given range.
    getRandomR :: (Random a) => (a, a) -> m a

instance MonadRandom IO where
    getRandom  = randomIO
    getRandomR = randomRIO
