-- |
-- Module:     Control.Wire.TimedMap
-- Copyright:  (c) 2012 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Timed maps for efficient cleanups in the context wires.

module Control.Wire.TimedMap
    ( -- * Timed maps
      TimedMap,
      -- * Queries
      findWithDefault,
      lookup,
      -- * Construction
      empty,
      -- * Insertion
      insert,
      -- * Deletion
      cleanup,
      cut,
      delete
    )
    where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Data
import Data.Map (Map)
import Data.Set (Set)
import Prelude hiding (lookup)


-- | A timed map is a map with an additional index based on time.

data TimedMap t k a =
    TimedMap !(Map k (a, t)) !(Map t (Set k))
    deriving (Data, Show, Typeable)


-- | Remove all elements older than the given time.

cleanup :: (Ord k, Ord t) => t -> TimedMap t k a -> TimedMap t k a
cleanup t0 (TimedMap mk' mt') = TimedMap mk mt
    where
    (older', middle, mt) = M.splitLookup t0 mt'
    older =
        M.fromDistinctAscList .
        map (, ()) .
        S.toList .
        M.foldl' S.union S.empty .
        maybe id (M.insert t0) middle $ older'
    mk = mk' M.\\ older


-- | Remove all but the given number of latest elements.

cut :: (Ord k, Ord t) => Int -> TimedMap t k a -> TimedMap t k a
cut n !tm@(TimedMap mk mt)
    | M.size mk > n =
        let k = S.findMin . snd . M.findMin $ mt in
        cut n (delete k tm)
    | otherwise = tm


-- | Deletes the given key from the timed map.

delete :: (Ord k, Ord t) => k -> TimedMap t k a -> TimedMap t k a
delete k (TimedMap mk' mt') = TimedMap mk mt
    where
    mk = M.delete k mk'
    mt = case M.lookup k mk' of
           Nothing     -> mt'
           Just (_, t') ->
               let alter Nothing = Nothing
                   alter (Just s') = do
                       let s = S.delete k s'
                       guard (not (S.null s))
                       return s
               in M.alter alter t' mt'


-- | Like 'lookup', but with a default value, if the key is not in the
-- map.

findWithDefault :: (Ord k) => (a, t) -> k -> TimedMap t k a -> (a, t)
findWithDefault def k = maybe def id . lookup k


-- | Empty timed map.

empty :: TimedMap t k a
empty = TimedMap M.empty M.empty


-- | Insert into the timed map.

insert :: (Ord k, Ord t) => t -> k -> a -> TimedMap t k a -> TimedMap t k a
insert t k x (TimedMap mk' mt') = TimedMap mk mt
    where
    mk = M.insert k (x, t) mk'
    mt = case M.lookup k mk' of
           Nothing      -> M.insertWith S.union t (S.singleton k) mt'
           Just (_, t') ->
               let alter Nothing = Nothing
                   alter (Just s') = do
                       let s = S.delete k s'
                       guard (not (S.null s))
                       return s
               in M.insertWith S.union t (S.singleton k) .
                  M.alter alter t' $ mt'


-- | Look up the given key in the timed map.

lookup :: (Ord k) => k -> TimedMap t k a -> Maybe (a, t)
lookup k (TimedMap mk _) = M.lookup k mk
