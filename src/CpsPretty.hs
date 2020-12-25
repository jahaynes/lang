module CpsPretty where

import Core
import Pretty
import SourceMap

import Data.ByteString (ByteString)

data CpsPretty a =
    CpsPretty (SourceMap ByteString) a deriving Show

--instance Show (CpsPretty a) where
    --show = paren . pretty 0

instance Show a => Pretty (CpsPretty a) where

    pretty n (CpsPretty sm (TopLevelEnv xs)) = error $ unlines . map (simple . pretty n) $ xs

    pretty n (CpsPretty sm expr) = error $ show expr


{- class Pretty a where
    pretty :: Int -> a -> (IsCompound, String) -}