-- |
-- Module:     Control.Wire.Trans.Embed
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Combinators for embedding wires.

module Control.Wire.Trans.Embed
    ( -- * Embedding wires
      embed
    )
    where

import Control.Wire.Wire


-- | Performs the argument wire with the input time delta.  It is
-- stepped often enough to catch up with the main wire.  The individual
-- results are combined as given by the fold (second and third
-- argument).
--
-- * Complexity: O(n) time wrt stepping the subwire, where n is the
--   number of times the subwire is stepped.
--
-- * Depends: like argument wire, if stepped.
--
-- * Inhibits: When the fold results in a 'Left'.

embed ::
    (Monad m)
    => (a -> Time)                -- ^ Time delta for the subwire.
    -> (Either e c -> Either e b -> Either e c)  -- ^ Folding function.
    -> Either e c                      -- ^ Fold base value.
    -> Wire e m a b               -- ^ Subwire to step.
    -> Wire e m a c
embed delta fold z = embed' 0
    where
    embed' rdt w0 =
        mkGen $ \dt x' ->
            let idt = delta x'
                loop odt r w'
                    | odt >= idt = do
                        (mx, w) <- stepWire w' idt x'
                        loop (odt - idt) (fold r mx) w
                    | otherwise = return (r, embed' odt w')
            in loop (rdt + dt) z w0
