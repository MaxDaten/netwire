-- |
-- Module:     Control.Wire.Session
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Wire sessions.

module Control.Wire.Session
    ( -- * Performing instants
      stepSession,
      stepSession_,
      stepSessionP,
      stepSessionP_,

      -- * Testing wires
      testWire,
      testWireP,
      -- ** Helper functions
      testPrint,

      -- * Sessions
      Session(..),
      -- ** Generic sessions
      genSession,
      -- ** Specific session types
      clockSession,
      counterSession,
      frozenSession
    )
    where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Wire.Types
import Control.Wire.Wire
import Data.Monoid
import Data.Time.Clock
import System.IO


-- | A session value contains time-related information.

newtype Session m =
    Session {
      sessionUpdate :: m (Time, Session m)
    }


-- | Construct a session using real time.  This session type uses
-- 'getCurrentTime'.  If you have a faster time source, you may want to
-- use 'genSession' instead and construct your own clock.

clockSession :: (MonadIO m) => Session m
clockSession =
    Session $ do
        t0 <- liftIO getCurrentTime
        return (0, loop t0)

    where
    loop t' =
        Session $ do
            t <- liftIO getCurrentTime
            let dt = realToFrac (diffUTCTime t t')
            return (dt, loop t)


-- | Construct a simple counter session.  The time delta is the given
-- argument at every instant.

counterSession ::
    (Monad m)
    => Time  -- ^ Time delta for every instant.
    -> Session m
counterSession dt =
    let s = Session (return (dt, s)) in s


-- | Construct a frozen session.  Same as @'counterSession' 0@.

frozenSession :: (Monad m) => Session m
frozenSession = counterSession 0


-- | Construct a generic session from the given initial session value
-- and the update function.  You can use this function to implement your
-- own clock.
--
-- If you just want to use real time, you may want to use
-- 'clockSession'.

genSession ::
    (Monad m)
    => a
    -> (a -> m (Time, a))
    -> Session m
genSession s' f =
    Session $ do
        (t, s) <- f s'
        return (t, genSession s f)


-- | Perform an instant of the given wire as part of a wire session.
--
-- This is a convenience function.  You can also construct time deltas
-- yourself entirely circumventing 'Session'.  This can be useful, if
-- there is really no need for an effectful monad.

stepSession ::
    (MonadIO m)
    => Wire e m a b  -- ^ Wire to step.
    -> Session m     -- ^ Current session state.
    -> a             -- ^ Input value.
    -> m (Either e b, Wire e m a b, Session m)
stepSession w' (Session update) x' = do
    (dt, s) <- update
    (mx, w) <- stepWire w' dt x'
    mx `seq` return (mx, w, s)


-- | Like 'stepSession', but throws an exception instead of returning an
-- 'Either' value.

stepSession_ ::
    (MonadIO m)
    => WireM m a b  -- ^ Wire to step.
    -> Session m    -- ^ Current session state.
    -> a            -- ^ Input value.
    -> m (b, WireM m a b, Session m)
stepSession_ w' s' x' = do
    (mx, w, s) <- stepSession w' s' x'

    let throwM   = liftIO . throwIO
        emptyErr = toException (userError "empty inhibition signal")
    x <- either (throwM . maybe emptyErr id . getLast) return mx

    return (x, w, s)


-- | Like 'stepSession', but for pure wires.

stepSessionP ::
    (Monad m)
    => Wire e Identity a b  -- ^ Wire to step.
    -> Session m            -- ^ Current session state.
    -> a                    -- ^ Input value.
    -> m (Either e b, Wire e Identity a b, Session m)
stepSessionP w' (Session update) !x' = do
    (dt, s) <- update
    let (mx, w) = stepWireP w' dt x'
    mx `seq` return (mx, w, s)


-- | Like 'stepSessionP', but throws an exception instead of returning an
-- 'Either' value.

stepSessionP_ ::
    (MonadIO m)
    => WireP a b  -- ^ Wire to step.
    -> Session m  -- ^ Current session state.
    -> a          -- ^ Input value.
    -> m (b, WireP a b, Session m)
stepSessionP_ w' s' !x' = do
    (mx, w, s) <- stepSessionP w' s' x'

    let throwM   = liftIO . throwIO
        emptyErr = toException (userError "empty inhibition signal")
    x <- either (throwM . maybe emptyErr id . getLast) return mx

    return (x, w, s)


-- | @testPrint n int mx@ prints a formatted version of @mx@ to stderr,
-- if @n@ is zero.  It returns @mod (succ n) int@.  Requires @n >= 0@ to
-- work properly.
--
-- This function is used to implement the /printing interval/ used in
-- 'testWire' and 'testWireM'.

testPrint :: (Show e) => Int -> Int -> Either e String -> IO Int
testPrint n' int mx = do
    let n = let nn = n' + 1 in
            if nn >= int then 0 else nn
    when (n' == 0) $ do
        hPutStr stderr "\r\027[K"
        hPutStr stderr (either (("(I) " ++) . show) id mx)
        hFlush stderr
    n `seq` return n


-- | Runs the given wire continuously and prints its result to stderr.
-- Runs forever until an exception is raised.
--
-- The /printing interval/ sets the instants/printing ratio.  The higher
-- this value, the less often the output is printed.  Examples:  1000
-- means to print at every 1000-th instant, 1 means to print at every
-- instant.

testWire ::
    forall a b e m. (MonadIO m, Show e)
    => Int                -- ^ Printing interval.
    -> Int                -- ^ 'threadDelay' between instants.
    -> m a                -- ^ Input generator.
    -> Session m          -- ^ Initial session value.
    -> Wire e m a String  -- ^ Wire to test.
    -> m b
testWire int delay getInput = loop 0
    where
    loop :: Int -> Session m -> Wire e m a String -> m b
    loop n' s' w' = do
        x' <- getInput
        (mx, w, s) <- stepSession w' s' x'
        n <- mx `seq` liftIO (testPrint n' int mx)
        when (delay > 0) (liftIO (threadDelay delay))
        loop n s w


-- | Like 'testWire', but for pure wires.

testWireP ::
    forall a b e m. (MonadIO m, Show e)
    => Int                       -- ^ Printing interval.
    -> Int                       -- ^ 'threadDelay' between instants.
    -> m a                       -- ^ Input generator.
    -> Session m                 -- ^ Initial session value.
    -> Wire e Identity a String  -- ^ Wire to test.
    -> m b
testWireP int delay getInput = loop 0
    where
    loop :: Int -> Session m -> Wire e Identity a String -> m b
    loop n' s' w' = do
        x' <- getInput
        (mx, w, s) <- stepSessionP w' s' x'
        n <- mx `seq` liftIO (testPrint n' int mx)
        when (delay > 0) (liftIO (threadDelay delay))
        loop n s w
