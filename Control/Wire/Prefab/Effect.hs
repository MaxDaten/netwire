-- |
-- Module:     Control.Wire.Prefab.Effect
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Effectful wires.

module Control.Wire.Prefab.Effect
    ( -- * Monadic effects
      -- ** Simple
      perform,
      -- ** Exception-aware
      execute,
      execute_,
      executeWith,
      executeWith_,

      -- * Branching
      branch,
      quit,
      quitWith
    )
    where

import qualified Data.Bifunctor as Bi
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control
import Control.Wire.Types
import Control.Wire.Wire
import Data.List
import Data.Monoid


-- | Branch according to the unterlying 'MonadPlus' instance.  Note that
-- this wire branches at every instant.
--
-- * Depends: current instant.

branch :: (MonadPlus m) => Wire e m [a] a
branch = mkFixM $ \_ -> liftM Right . foldl' mplus mzero . map return


-- | Variant of 'executeWith' for the 'LastException' inhibition monoid.
--
-- * Depends: current instant.
--
-- * Inhibits: when the action throws an exception.

execute ::
    (MonadBaseControl IO m)
    => Wire LastException m (m a) a
execute = executeWith (Last . Just)


-- | Variant of 'executeWith_' for the 'LastException' inhibition monoid.
--
-- * Depends: current instant, if the given function is strict.
--
-- * Inhibits: when the action throws an exception.

execute_ ::
    (MonadBaseControl IO m)
    => (a -> m b)
    -> Wire LastException m a b
execute_ = executeWith_ (Last . Just)


-- | Perform the input monadic action at every instant.
--
-- * Depends: current instant.
--
-- * Inhibits: when the action throws an exception.

executeWith ::
    (MonadBaseControl IO m)
    => (SomeException -> e)  -- ^ Turns an exception into an inhibition value.
    -> Wire e m (m a) a
executeWith fromEx = mkFixM $ \_ c -> liftM (Bi.first fromEx) (try c)


-- | Perform the given monadic action at every instant.
--
-- * Depends: current instant, if the given function is strict.
--
-- * Inhibits: when the action throws an exception.

executeWith_ ::
    (MonadBaseControl IO m)
    => (SomeException -> e)  -- ^ Turns an exception into an inhibition value.
    -> (a -> m b)            -- ^ Action to perform.
    -> Wire e m a b
executeWith_ fromEx c = mkFixM $ \_ -> liftM (Bi.first fromEx) . try . c


-- | Perform the input monadic action in a wire.
--
-- * Depends: current instant.

perform :: (Monad m) => Wire e m (m b) b
perform = mkFixM . const $ liftM Right


-- | Quits the current branch using 'mzero'.

quit :: (MonadPlus m) => Wire e m a b
quit = mkFixM $ \_ _ -> mzero


-- | Acts like identity in the first instant, then quits the current
-- branch using 'mzero'.
--
-- * Depends: first instant.

quitWith :: (MonadPlus m) => Wire e m a a
quitWith = mkPure $ \_ x -> (Right x, quit)
