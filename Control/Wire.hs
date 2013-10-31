-- |
-- Module:     Control.Wire
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Netwire is a library for functional reactive programming, that is for
-- time-varying values.  It allows you to express various reactive
-- systems elegantly and concisely by using an embedded domain-specific
-- language.  Examples of such systems include
--
-- * games,
--
-- * network applications with time-varying components,
--
-- * simulations,
--
-- * stateful web applications,
--
-- * widget-based user interfaces.
--
-- This library is based on an extension of the automaton arrow.  The
-- usage is explained in the following tutorial.

module Control.Wire
    ( -- * Quickstart tutorial
      -- $quickstart_intro

      -- ** Running wires
      -- $quickstart_running

      -- ** Constructing wires
      -- $quickstart_constructing

      -- ** Signal inhibition and events
      -- $quickstart_inhibition

      -- ** Custom wires
      -- $quickstart_custom

      -- * Netwire reexports
      module Control.Wire.Classes,
      module Control.Wire.Prefab,
      module Control.Wire.Session,
      module Control.Wire.Trans,
      module Control.Wire.Types,
      module Control.Wire.Wire,

      -- * Other reexports
      module Control.Applicative,
      module Control.Arrow,
      module Control.Category,
      module Data.Proxy,
      module System.Random,
      Profunctor(..),
      Exception(..),
      SomeException(..)
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception (Exception(..), SomeException(..))
import Control.Wire.Classes
import Control.Wire.Prefab
import Control.Wire.Session
import Control.Wire.Trans
import Control.Wire.Types
import Control.Wire.Wire hiding (constant, identity, never)
import Data.Profunctor
import Data.Proxy (Proxy(..))
import System.Random


{- $quickstart_intro

This section is a quickstart tutorial for the experienced, impatient
Haskell programmer.

The main concept used in Netwire is a family of /wire categories/:

> data Wire e m a b

A value of type @Wire e m a b@ represents a function that takes as
arguments

* a time delta of type 'Time' (which is just 'Double') that will be
  explained below,

* an input value of type @a@.

From these inputs it

* either produces an output value of type @b@ or /inhibits/ with a value
  of type @e@,

* produces a new wire of type @Wire e m a b@.

So you can think of 'Wire' as:

> newtype Wire e m a b =
>     Wire {
>       stepWire :: Time -> a -> m (Either e b, Wire e m a b)
>     }

To summarize a wire of type @Wire e m a b@ takes a value of type @a@ and
supposedly produces a value of type @b@.  It can be invoked multiple
times, where each invocation is called an /instant/ and the wire can
behave differently at every instant.  Additionally it can choose not to
produce anything, but instead inhibit with an /inhibition exception/ of
type @e@.  This is Netwire's notion of a time-varying value.  -}


{- $quickstart_running

To actually invoke a wire you can use the 'stepWire' function
(simplified type):

> stepWire ::
>     (Monad m) =>
>     Wire e m a b ->
>     Time ->
>     a ->
>     m (Either e b, Wire e m a b)

The idea is simple:  You have an application loop that invokes a given
wire with a time delta, which is just the number of seconds passed since
the last instant and an application-specific input value.  It then does
something with the output value (or inhibition value) and restarts the
loop with the new wire produced by the current wire.  Such an
application loop based on @stepWire@ could look like this:

> loop w' = do
>     dt <- timeDeltaToLastInstant
>     (mx, w) <- stepWire w' dt ()
>     case mx of
>       Left ex -> printf "Inhibited: %s\n" (show ex)
>       Right x -> printf "Produced: %s\n" (show x)
>     loop w

Usually the time deltas are based on actual clock time.  To simplify
invocation for this common case there is a set of convenience functions
like 'stepSession' for stepping that calculate the time deltas for you:

> stepSession ::
>     (Monad m) =>
>     Wire e m a b ->
>     Session m ->
>     a ->
>     m (Either e b, Wire e m a b, Session m)

To construct the initial session value you can use 'clockSession' or one
of the other predefined intial session values:

> clockSession :: (MonadIO m) => Session m

This simplifies the application loop, because you don't have to
calculate the time deltas yourself:

> loop w' session' = do
>     (mx, w, session) <- stepSession w' session' ()
>     case mx of
>       Left ex -> printf "Inhibited: %s\n" (show ex)
>       Right x -> printf "Produced: %s\n" (show x)
>     loop w session

For the common case where the wire's underlying monad is 'Identity', but
the application monad is something else, there are convenience functions
like 'stepWireP', 'stepSessionP' and other @*P@ variants.

We haven't covered constructing wires yet.  This is explained in the
next section.  But we now have everything necessary to write our first
small application:

> module Main where
>
> import Control.Monad.Identity (Identity)
> import Control.Wire
> import Prelude hiding ((.), id)
> import Text.Printf
>
> testApp :: Wire () Identity a Time
> testApp = timeFrom 10
>
> main :: IO ()
> main = loop testApp clockSession
>     where
>     loop w' session' = do
>         (mx, w, session) <- stepSessionP w' session' ()
>         case mx of
>           Left ex -> putStrLn ("Inhibited: " ++ show ex)
>           Right x -> putStrLn ("Produced: " ++ show x)
>         loop w session

When you run this program, it will continuously display a number of
seconds starting with 10.  That's the @timeFrom 10@ wire.  Notice that
the "Prelude" module is imported with hidden 'Prelude..' and
'Prelude.id'.  Don't worry, the "Control.Wire" module reexports the
"Control.Category" module, which includes generalized version of both.
-}


{- $quickstart_constructing

A number of convenience types are defined in the "Control.Wire.Types"
module, in particular the 'WireP' type:

> type WireP = Wire LastException Identity

Wires can be composed categorically, applicatively or by using wire
combinators.  To feed the output of one wire @w1@ into another wire @w2@
you just use categorical composition:

> w2 . w1

For example the 'noise' wire generates random noise based on the given
random number generator.  If its output type is 'Double', it generates
noise 0 <= x t < 1.  The 'avg' wire calculates the average value of its
input over the last given number of samples:

> let myNoise = noise (mkStdGen 0) :: WireP a Double
>     myAvg   = avg 1000
> in myAvg . myNoise

That wire should produce values near 0.5, the average noise value over
the last 1000 samples of random noise between 0 and 1.  There is a bit
of cruft here to tell the type system that noise's output type is
'Double'.  To make this easier you can simply use 'outAs' or 'inAs':

> avg 1000 . outAs pDouble (noise (mkStdGen 0))

The 'Wire' type gives rise to a family of applicative functors.  Using
applicative style you can apply a function to the output of a wire or
zip together the outputs of two wires (the "Control.Applicative" module
is reexported by this module):

> timeString = fmap (printf "%8.2f") time
>
> noisyTime = liftA2 (+) time (noise (mkStdGen 0))

Constant wires can be produced using 'pure'.  The following wire starts
at 0 and increases with a constant speed of 3:

> integral_ 0 . pure 3

There are lots of convenience instances for wires.  For example there
are instances for 'Num', 'Fractional' and 'Data.String.IsString', so you
can actually just use regular arithmetical operators and numeric
literals.  If you have enabled the @OverloadedStrings@ extension you can
also write string literals:

> let n = noise (mkStdGen 0)
> in time + 3*n
>
> integral_ 0 . 3

There is a large library of predefined wires below the
"Control.Wire.Prefab" tree.
-}


{- $quickstart_inhibition

As noted a few times wires can choose not to produce a value.  In those
cases the wire /inhibits/ the signal.  This is where the @e@ type comes
into play.  That type is called the /inhibition monoid/.

Signal inhibition is what makes Netwire different.  The 'Wire' type is
an 'Alternative' functor, where the 'empty' wire always inhibits and
wires can be combined with the following semantics:

> w1 <|> w2

If @w1@ inhibits, then the combination @w1 \<|\> w2@ acts like @w2@.  In
other words, the combination chooses the first wire that produces.  If
both inhibit, then the combination inhibits.

Events are modelled around this.  An event wire is usually a wire that
acts like the identity wire, but it may inhibit depending on whether an
event has occurred or not.  One simple event wire is the 'for' wire:

> for 3

This wire acts like the identity wire for three seconds and then stops
producing forever.  You can use it to construct a wire that produces
"yes" for three seconds and then switches to "no":

> "yes" . for 3 <|> "no"

Another useful event wire is the 'wackelkontakt' wire (a Netwire running
gag; it's the German word for slack joint):

> wackelkontakt 0.9

This wire acts like the identity wire most of the time (90%), but
occasionally inhibits (10%).  Using it you can produce a broken clock,
which occasionally refuses to display the current time:

> brokenClock =
>     printf "%8.2f" <$> wackelkontakt 0.9 . time <|>
>     "sorry, slack joint"

The 'periodically' wire produces once every given number of seconds.
The following wire produces once every two seconds:

> periodically 2

There are various combinators for event wires in the
"Control.Wire.Trans.Event" module, most notably 'hold' and 'holdFor'.
Given an inhibiting wire the @hold@ combinator holds the last produced
value, so it turns instantaneous events into continuous ones:

> secondClock = printf "%8.2f" <$> hold (periodically 1 . time)

This one displays the time in seconds and is only updated every second.
The 'holdFor' combinator allows you to limit the time the last output is
held for:

> jumpyClock =
>     printf "%8.2f" <$> holdFor 0.5 (periodically 1 . time) <|>
>     "wait 500ms for the next second"

You find a library of predefined event wires in the
"Control.Wire.Prefab.Event" module.
-}


{- $quickstart_custom

From time to time you will want to write your own wire on a lower level.
In this case there are a number of options.  The simplest option is
'mkPure':

> mkPure ::
>     (Time -> a -> (Either e b, Wire e m a b)) ->
>     Wire e m a b

The type quite literally tells what this function does.  It takes a
function and turns it into a wire quite straightforwardly.  Another
option is to use 'mkState', which is equivalent to @mkPure@, but allows
you to express the wire as a local state transformer:

> mkState ::
>     s ->
>     (Time -> (a, s) -> (Either e b, s)) ->
>     Wire e m a b

The first argument is a starting state, the second is the state
transformation function.
-}
