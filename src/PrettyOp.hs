module PrettyOp where

import Core
import Pretty

newtype PrettyUnOp =
    PrettyUnOp UnOp

instance Pretty PrettyUnOp where
    pretty _ (PrettyUnOp EShow) = simple "Show"
    pretty _ (PrettyUnOp (Err)) = simple $ "ERROR: "

newtype PrettyBinOp =
    PrettyBinOp BinOp

instance Pretty PrettyBinOp where
    pretty _ (PrettyBinOp AddI)    = simple "+"
    pretty _ (PrettyBinOp SubI)    = simple "-"
    pretty _ (PrettyBinOp MulI)    = simple "*"
    pretty _ (PrettyBinOp DivI)    = simple "/"
    pretty _ (PrettyBinOp ModI)    = simple "%"
    pretty _ (PrettyBinOp EqI)     = simple "=="
    pretty _ (PrettyBinOp ConcatS) = simple "++"
    pretty _ (PrettyBinOp LtEqI)   = simple "<="
    pretty _ (PrettyBinOp LtI)     = simple "<"
    pretty _ (PrettyBinOp GtEqI)   = simple ">="
    pretty _ (PrettyBinOp GtI)     = simple ">"
    
-- TODO - represent Pretty(CpsEpxr) so it looks like SSA?
