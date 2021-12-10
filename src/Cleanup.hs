module Cleanup where

import Core

import Data.ByteString (ByteString)

type S = ByteString

cleanup :: Defn S -> Defn S
cleanup (FunDef n e) = FunDef n (goExp e)

    where
    goExp (ELam [] body) =
        goExp body

    goExp (ELam vs body) =
        ELam vs (goExp body)

    goExp (EApp f []) =
        goExp f

    goExp (EApp f xs) =
        EApp (goExp f) (map goExp xs)

    goExp (ELet a b c) =
        ELet a (goExp b) (goExp c)

    goExp (EBinPrimOp op a b) =
        EBinPrimOp op (goExp a) (goExp b)

    goExp (EUnPrimOp op a) =
        EUnPrimOp op (goExp a)

    -- could opt here if b is known
    goExp (IfThenElse b t f) =
        IfThenElse (goExp b) (goExp t) (goExp f)

    goExp t@ETerm{} =
        t

    goExp x =
        error $ show ("goExp", x)

cleanup x = x