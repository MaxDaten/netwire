-- |
-- Module:     Control.Wire.Types
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Types used in Netwire.  Most notably this module implements the
-- instances for the various reactive classes.

module Control.Wire.Types
    ( -- * Convenient type aliases
      LastException,
      -- ** Events
      Event,
      EventM,
      EventP,
      -- ** Wires
      WireM,
      WireP,

      -- * Type-related utilities
      as,
      inAs,
      inLike,
      like,
      outAs,
      outLike,
      -- ** Predefined proxies
      pDouble,
      pFloat,
      pInt,
      pInteger,
      pString
    )
    where

import Control.Category
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Identity
import Control.Wire.Wire
import Data.Monoid
import Data.Proxy
import Prelude hiding ((.), id)


-- | Event wires are wires that act like identity wires, but may inhibit
-- depending on whether a certain event has occurred.

type Event e m a = Wire e m a a


-- | 'WireP' equivalent of 'Event'.

type EventP a = WireP a a


-- | 'WireM' equivalent of 'Event'.

type EventM m a = WireM m a a


-- | Monoid for the last occurred exception.

type LastException = Last SomeException


-- | Monadic wires using 'LastException' as the inhibition monoid.

type WireM = Wire LastException


-- | Pure wires using 'LastException' as the inhibition monoid.

type WireP = WireM Identity


-- | Type-restricted identity wire.  This is useful to specify the type
-- of a signal.
--
-- * Depends: current instant.

as :: (Monad m) => Proxy a -> Wire e m a a
as _ = id


-- | Utility to specify the input type of a wire.  The argument is
-- ignored.  For types with defaulting you might prefer 'inLike'.
--
-- > inAs (Proxy :: Proxy Double) highPeak

inAs :: Proxy a -> w a b -> w a b
inAs = const id


-- | Utility to specify the input type of a wire.  The first argument is
-- ignored.  This is useful to make use of defaulting or when writing a
-- dummy value is actually shorter.
--
-- > inLike (0 :: Double) highPeak

inLike :: a -> w a b -> w a b
inLike = const id


-- | Type-restricted identity wire.  This is useful to specify the type
-- of a signal.  The argument is ignored.
--
-- * Depends: current instant.

like :: (Monad m) => a -> Wire e m a a
like = const id


-- | Utility to specify the output type of a wire.  The argument is
-- ignored.  For types with defaulting you might prefer 'outLike'.
--
-- > outAs (Proxy :: Proxy Double) noiseM

outAs :: Proxy b -> w a b -> w a b
outAs = const id


-- | Utility to specify the output type of a wire.  The first argument
-- is ignored.  This is useful to make use of defaulting or when writing
-- a dummy value is actually shorter.
--
-- > outLike (0 :: Double) noiseM

outLike :: b -> w a b -> w a b
outLike = const id


-- | 'Double' proxy for use with 'inAs' or 'outAs'.

pDouble :: Proxy Double
pDouble = Proxy


-- | 'Float' proxy for use with 'inAs' or 'outAs'.

pFloat :: Proxy Float
pFloat = Proxy


-- | 'Int' proxy for use with 'inAs' or 'outAs'.

pInt :: Proxy Int
pInt = Proxy


-- | 'Integer' proxy for use with 'inAs' or 'outAs'.

pInteger :: Proxy Integer
pInteger = Proxy


-- | 'String' proxy for use with 'inAs' or 'outAs'.

pString :: Proxy String
pString = Proxy
