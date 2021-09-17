{-# LANGUAGE OverloadedStrings #-}

module Alphabetise where

import Core
import SourceMap
import State

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Map              (Map)
import qualified Data.Map as M

data AlphaState = AlphaState (Map ByteString ByteString) Int (SourceMap ByteString)

alphabetise :: [Defn ByteString] -> ([Defn ByteString], SourceMap ByteString)
alphabetise defns =
    (\(AlphaState _ _ sm) -> sm) <$> runState (mapM alphabetiseDefn defns) (AlphaState M.empty 1 empty)

    where
    alphabetiseDefn :: Defn ByteString -> State AlphaState (Defn ByteString)
    alphabetiseDefn   (FunDef name expr) = FunDef <$> pure name <*> go expr
    alphabetiseDefn d@(DataDef _ _)    = pure d -- No scoping needed for type variable?

    go :: Expr ByteString -> State AlphaState (Expr ByteString)
    go t@(ETerm (Var v)) = do
        AlphaState env _ _ <- get
        pure $ case M.lookup v env of
                    -- Found in local scope
                    Just v' -> ETerm (Var v')
                    Nothing ->
                        case filter (\(FunDef n _) -> n == v) (filter isFunDef defns) of
                        []  -> error $ "Symbol " ++ show v ++ " not found in local scope or top-level function definition"
                        [_] -> t -- Reference to one top-level definition
                        _   -> error $ "Ambiguous symbol " ++ show v ++ " could refer to more than one top-level function definition"

    go t@(ETerm (DCons _)) = pure t

    go t@(ETerm (   LitInt _)) = pure t
    go t@(ETerm (  LitBool _)) = pure t
    go t@(ETerm (LitString _)) = pure t

    go (ELet a b c) = do
        a' <- scope a
        b' <- go b
        c' <- go c
        descope a
        pure $ ELet a' b' c'

    go (ELam vs b) = do
        vs' <- mapM scope vs
        b'  <- go b
        mapM_ descope vs
        pure $ ELam vs' b'

    go (EApp a bs) = do
        a'  <- go a
        bs' <- mapM go bs
        pure $ EApp a' bs'

    go (EUnPrimOp op e) = EUnPrimOp op <$> go e

    go (EBinPrimOp op a b) = EBinPrimOp op <$> go a <*> go b

    go (IfThenElse p t f) = IfThenElse <$> go p <*> go t <*> go f

    scope :: ByteString -> State AlphaState ByteString
    scope v = do
        AlphaState env i sm <- get
        let v' = pack $ "v" ++ show i
            env' = M.insert v v' env
            sm' = insert v' v sm
        put $ AlphaState env' (i + 1) sm'
        pure v'

    descope :: ByteString -> State AlphaState ()
    descope v = do
        AlphaState env i sm <- get
        let env' = M.delete v env
        put $ AlphaState env' i sm

    isFunDef :: Defn s -> Bool
    isFunDef (FunDef _ _) = True
    isFunDef            _ = False