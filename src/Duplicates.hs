{-# LANGUAGE Strict #-}

module Duplicates (
    buildMap, filterMap, printMap
) where

import Data.HashMap.Strict qualified as Map
import Data.HashSet (singleton, size, union)
import Tree (DirectoryTree (..), MetaInformation (..))
import Data.Tree (Tree (Node), drawForest)
import Utils (shortHash)

type EntryMap = HashMap ByteString (HashSet Text)

buildMap :: EntryMap -> DirectoryTree ByteString -> EntryMap
{-# INLINEABLE buildMap #-}
buildMap map = \case
    File (Meta n h) -> Map.insertWith union h (singleton n) map
    Directory _ e -> foldl' (Map.unionWith union) map (buildMap mempty <$> toList e)

filterMap :: EntryMap -> EntryMap
{-# INLINEABLE filterMap #-}
filterMap = Map.filter (\s -> size s > 1)

printMap :: EntryMap -> String
{-# INLINABLE printMap #-}
printMap m =
    let
        convertEntry (h, ps) = Node (shortHash h) $ (\p -> Node (toString p) []) <$> toList ps
        entries = convertEntry <$> Map.toList m
    in
        drawForest entries