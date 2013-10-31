-- |
-- Module:     Control.Wire.Trans
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Proxy module to all wire combinator modules.

module Control.Wire.Trans
    ( -- * Reexports
      module Control.Wire.Trans.Combine,
      module Control.Wire.Trans.Embed,
      module Control.Wire.Trans.Event,
      module Control.Wire.Trans.Simple,
      module Control.Wire.Trans.Switch,
      module Control.Wire.Trans.Time
    )
    where

import Control.Wire.Trans.Combine
import Control.Wire.Trans.Embed
import Control.Wire.Trans.Event
import Control.Wire.Trans.Simple
import Control.Wire.Trans.Switch
import Control.Wire.Trans.Time
