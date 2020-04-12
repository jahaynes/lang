{-# LANGUAGE OverloadedStrings #-}

module SourceMap where

import           Data.Map        (Map)
import qualified Data.Map as M
import           Data.Maybe      (fromMaybe)

newtype SourceMap s = SourceMap (Map s s) deriving Show

empty :: SourceMap s
empty = SourceMap M.empty

insert :: Ord s => s -> s -> SourceMap s -> SourceMap s
insert alphad orig (SourceMap m) = SourceMap (M.insert alphad orig m)

lookup :: Ord s => s -> SourceMap s -> Maybe s
lookup alphad (SourceMap m) = M.lookup alphad m

lookupDef :: Ord s => s -> SourceMap s -> s
lookupDef alphad (SourceMap m) = 
    fromMaybe alphad (M.lookup alphad m)
