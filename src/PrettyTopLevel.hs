module PrettyTopLevel where

import Core
import Pretty
import PrettyExpr
import SourceMap (SourceMap, lookupDef)

import Data.ByteString.Char8 (ByteString, unpack)

data PrettyTopLevel =
    PrettyTopLevel (SourceMap ByteString) (TopLevelEnv ByteString)

instance Show (PrettyTopLevel) where
    show = paren . pretty 0

instance Pretty PrettyTopLevel where
    pretty _ (PrettyTopLevel sm (TopLevelEnv tle)) =
        let ls = (\(k,v) -> unpack (lookupDef k sm) <> ": " <> show (PrettyExpr sm v)) <$> tle
        in simple (unlines ls)