-- |
-- Module:     Control.Wire.Wire
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- This is the core module implementing the 'Wire' type.

module Control.Wire.Wire
    ( -- * Wires
      Wire(..),
      Time,
      -- ** Constructing wires
      mkFix,
      mkFixM,
      mkGen,
      mkPure,
      mkState,
      mkStateM,
      -- ** Simple predefined wires
      constant,
      identity,
      never,
      -- ** Simple predefined combinators
      cons,
      fixW,
      mapOutput,

      -- * Stepping
      stepWire,
      stepWireP
    )
    where

import qualified Data.Bifunctor as Bi
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Cross
import Data.Monoid
import Data.Profunctor
import Data.String
import Data.VectorSpace
import Prelude hiding ((.), id)


-- | Time.

type Time = Double


-- | A wire is a signal function from an input value of type @a@ that
-- either /produces/ an output value of type @b@ or /inhibits/ with a
-- value of type @e@.  The underlying monad is @m@.

data Wire e m a b
    = WGen (Time -> a -> m (Either e b, Wire e m a b))
    | WPure (Time -> a -> (Either e b, Wire e m a b))

instance (AdditiveGroup b, Monad m) => AdditiveGroup (Wire e m a b) where
    zeroV = pure zeroV
    (^+^) = liftA2 (^+^)
    negateV = fmap negateV

instance (AdditiveGroup (Diff b), AffineSpace b, Monad m) => AffineSpace (Wire e m a b) where
    type Diff (Wire e m a b) = Wire e m a (Diff b)
    (.-.) = liftA2 (.-.)
    (.+^) = liftA2 (.+^)

instance (Monad m, Monoid e) => Alternative (Wire e m a) where
    empty = mkFix (const . const $ Left mempty)

    (<|>) = loop 0
        where
        loop !t2 (WPure f1) w2'@(WPure f2) =
            mkPure $ \dt x' ->
                let (mx1, w1) = f1 dt x' in
                case mx1 of
                  Left ex1 ->
                      let (mx2, w2) = f2 (t2 + dt) x' in
                      (Bi.first (mappend ex1) mx2, loop 0 w1 w2)
                  Right _ -> (mx1, loop (t2 + dt) w1 w2')

        loop !t2 w1' w2' =
            mkGen $ \dt x' -> do
                (mx1, w1) <- stepWire w1' dt x'
                case mx1 of
                  Left ex1 -> do
                      (mx2, w2) <- stepWire w2' (t2 + dt) x'
                      return (Bi.first (mappend ex1) mx2, loop 0 w1 w2)
                  Right _ -> return (mx1, loop (t2 + dt) w1 w2')

instance (Monad m) => Applicative (Wire e m a) where
    pure = constant

    (<*>) = loop 0
        where
        loop !tx (WPure ff) wx'@(WPure fx) =
            mkPure $ \dt x' ->
                let (mf, wf) = ff dt x' in
                case mf of
                  Right f ->
                      let (mx, wx) = fx (tx + dt) x' in
                      (fmap f mx, loop 0 wf wx)
                  Left ex -> (Left ex, loop (tx + dt) wf wx')

        loop !tx wf' wx' =
            mkGen $ \dt x' -> do
                (mf, wf) <- stepWire wf' dt x'
                case mf of
                  Right f -> do
                      (mx, wx) <- stepWire wx' (tx + dt) x'
                      return (fmap f mx, loop 0 wf wx)
                  Left ex -> return (Left ex, loop (tx + dt) wf wx')

instance (Monad m) => Arrow (Wire e m) where
    arr f     = mkFix (const $ Right . f)
    first w   = liftA2 (,) (lmap fst w) (arr snd)
    second w  = liftA2 (,) (arr fst) (lmap snd w)
    (&&&)     = liftA2 (,)
    w1 *** w2 = liftA2 (,) (lmap fst w1) (lmap snd w2)

instance (Monad m) => ArrowChoice (Wire e m) where
    (|||) = loop 0 0
        where
        loop !tl !tr wl' wr' =
            mkGen $ \dt ->
                either (\x' -> do
                            (mx, wl) <- stepWire wl' (tl + dt) x'
                            return (mx, loop 0 (tr + dt) wl wr'))
                       (\x' -> do
                            (mx, wr) <- stepWire wr' (tr + dt) x'
                            return (mx, loop (tl + dt) 0 wl' wr))

    w1 +++ w2 = fmap Left w1 ||| fmap Right w2

    left = loop 0
        where
        loop !tl wl' =
            mkGen $ \dt ->
                either (liftM (fmap Left *** loop 0) . stepWire wl' (tl + dt))
                       (\x -> return (Right (Right x), loop (tl + dt) wl'))

    right = loop 0
        where
        loop !tr wr' =
            mkGen $ \dt ->
                either (\x -> return (Right (Left x), loop (tr + dt) wr'))
                       (liftM (fmap Right *** loop 0) . stepWire wr' (tr + dt))

instance (MonadFix m) => ArrowLoop (Wire e m) where
    loop w' =
        mkGen $ \dt x' ->
            liftM (fmap fst *** loop) .
            mfix $ \ ~(mx, _) ->
                let feedbackErr = error "Feedback loop broken by inhibition" in
                stepWire w' dt (x', either (const feedbackErr) snd mx)

instance (Monad m, Monoid e) => ArrowPlus (Wire e m) where
    (<+>) = (<|>)

instance (Monad m, Monoid e) => ArrowZero (Wire e m) where
    zeroArrow = empty

instance (Monad m) => Category (Wire e m) where
    id = identity

    (.) = loop 0
        where
        loop !t2 w2'@(WPure f2) (WPure f1) =
            mkPure $ \dt x'' ->
                let (mx', w1) = f1 dt x'' in
                case mx' of
                  Right x' ->
                      let (mx, w2) = f2 (t2 + dt) x' in
                      (mx, loop 0 w2 w1)
                  Left ex -> (Left ex, loop (t2 + dt) w2' w1)

        loop !t2 w2' w1' =
            mkGen $ \dt x'' -> do
                (mx', w1) <- stepWire w1' dt x''
                case mx' of
                  Right x' -> do
                      (mx, w2) <- stepWire w2' (t2 + dt) x'
                      return (mx, loop 0 w2 w1)
                  Left ex -> return (Left ex, loop (t2 + dt) w2' w1)

instance (Floating b, Monad m) => Floating (Wire e m a b) where
    pi = pure pi
    sqrt = fmap sqrt

    (**) = liftA2 (**)
    exp = fmap exp
    log = fmap log
    logBase = liftA2 logBase

    cos  = fmap cos;  sin  = fmap sin;  tan  = fmap tan
    acos = fmap acos; asin = fmap asin; atan = fmap atan

    cosh  = fmap cosh;  sinh  = fmap sinh;  tanh  = fmap tanh
    acosh = fmap acosh; asinh = fmap asinh; atanh = fmap atanh

instance (Fractional b, Monad m) => Fractional (Wire e m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
    recip = fmap recip

instance (Monad m) => Functor (Wire e m a) where
    fmap = mapOutput . fmap

instance (HasCross2 b, Monad m) => HasCross2 (Wire e m a b) where
    cross2 = fmap cross2

instance (HasCross3 b, Monad m) => HasCross3 (Wire e m a b) where
    cross3 = liftA2 cross3

instance (HasNormal b, Monad m) => HasNormal (Wire e m a b) where
    normalVec = fmap normalVec

instance (InnerSpace b, Monad m) => InnerSpace (Wire e m a b) where
    (<.>) = liftA2 (<.>)

instance (Monad m, Num b) => Num (Wire e m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)

    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (IsString b, Monad m) => IsString (Wire e m a b) where
    fromString = pure . fromString

instance (Monad m, Monoid b) => Monoid (Wire e m a b) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance (Monad m) => Profunctor (Wire e m) where
    lmap f (WPure g) = WPure (\dt -> second (lmap f) . g dt . f)
    lmap f (WGen g)  = WGen  (\dt -> liftM (second (lmap f)) . g dt . f)

    rmap = fmap

instance (Monad m, Read b) => Read (Wire e m a b) where
    readsPrec n = map (first pure) . readsPrec n

instance (Monad m, VectorSpace b) => VectorSpace (Wire e m a b) where
    type Scalar (Wire e m a b) = Wire e m a (Scalar b)
    (*^) = liftA2 (*^)


-- | Wire cons.  Prepend the given value to the wire's output stream.
-- This function is infixr like (':').
--
-- * Depends: like argument wire after the first instant.
--
-- * Inhibits: like argument wire after the first instant.

cons :: b -> Wire e m a b -> Wire e m a b
cons x xs = mkPure (\_ _ -> (Right x, xs))

infixr 5 `cons`


-- | Variant of 'pure' without the 'Monad' constraint.  Using 'pure' is
-- preferable.

constant :: b -> Wire e m a b
constant = mkFix . const . const . Right


-- | Convenience combinator for the common case of feedback where you
-- ignore the input and produce the feedback value itself with 'loop'.
--
-- * Depends: like looped argument wire.
--
-- * Inhibits: when argument wire inhibits.

fixW :: (MonadFix m) => Wire e m b b -> Wire e m a b
fixW = loop . fmap (\x -> (x, x)) . lmap snd


-- | Variant of 'id' without the 'Monad' constraint.  Using 'id' is
-- preferable.
--
-- * Depends: current instant.

identity :: Wire e m a a
identity = WPure (\_ x -> (Right x, identity))


-- | Map the given function over the raw wire output.

mapOutput :: (Monad m) => (Either e b' -> Either e b) -> Wire e m a b' -> Wire e m a b
mapOutput f (WGen g)  = WGen  (\dt -> liftM (f *** mapOutput f) . g dt)
mapOutput f (WPure g) = WPure (\dt -> (f *** mapOutput f) . g dt)


-- | Construct a pure stateless wire from the given function.

mkFix :: (Time -> a -> Either e b) -> Wire e m a b
mkFix f = let w = mkPure (\dt -> (, w) . f dt) in w


-- | Construct a stateless effectful wire from the given function.

mkFixM :: (Monad m) => (Time -> a -> m (Either e b)) -> Wire e m a b
mkFixM f = let w = mkGen (\dt -> liftM (, w) . f dt) in w


-- | Construct an effectful wire from the given function.

mkGen :: (Time -> a -> m (Either e b, Wire e m a b)) -> Wire e m a b
mkGen = WGen


-- | Construct a pure wire from the given function.

mkPure :: (Time -> a -> (Either e b, Wire e m a b)) -> Wire e m a b
mkPure = WPure


-- | Construct a pure wire from the given local state transision
-- function.

mkState ::
    s
    -> (Time -> (a, s) -> (Either e b, s))
    -> Wire e m a b
mkState s0 f = loop s0
    where
    loop s' =
        mkPure $ \dt x' ->
            let (mx, s) = f dt (x', s') in
            (mx, loop s)


-- | Construct a monadic wire from the given local state transision
-- function.

mkStateM ::
    (Monad m)
    => s
    -> (Time -> (a, s) -> m (Either e b, s))
    -> Wire e m a b
mkStateM s0 f = loop s0
    where
    loop s' =
        mkGen $ \dt x' -> liftM (second loop) (f dt (x', s'))


-- | Variant of 'empty' without the 'Monad' constraint.  Using 'empty'
-- is preferable.

never :: (Monoid e) => Wire e m a b
never = mkFix . const . const $ Left mempty


-- | Perform an instant of the given wire.

stepWire :: (Monad m) => Wire e m a b -> Time -> a -> m (Either e b, Wire e m a b)
stepWire (WGen f)  dt = f dt
stepWire (WPure f) dt = return . f dt


-- | Perform an instant of the given pure wire.

stepWireP :: Wire e Identity a b -> Time -> a -> (Either e b, Wire e Identity a b)
stepWireP (WGen f)  dt = runIdentity . f dt
stepWireP (WPure f) dt = f dt
