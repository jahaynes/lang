{-# LANGUAGE OverloadedStrings #-}

module CpsTransform (cpsLangify) where

import Core
import CpsLang
import CpsPrint
import State

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List                   (intercalate)
import           Data.Set                    (Set, (\\))
import qualified Data.Set as S
import           Text.Printf                 (printf)

data CpsConv =
    CpsConv { contNum :: !Int
            , lifted  :: ![(S, Val)]
            }

cpsLangify :: TopLevelEnv S -> [(S, Val)]
cpsLangify (TopLevelEnv tles) =
    let (as, CpsConv _ bs) = runState (mapM go tles) (CpsConv 0 [])
    in bs ++ as
    where
    go ("main", ELam vs body) = do
        body' <- toCpsC body (CVar "halt")
        pure ("main", CFunDef vs body')

    -- A lambda is the 'usual' case
    go (n, ELam vs body) = do
        k <- genvar
        body' <- toCpsC body (CVar k)
        pure (n, CFunDef (vs++[k]) body')

    -- A term can become a value
    go (n, e@ETerm{}) = do
        e' <- toVal e 
        pure (n, e')

    -- any other expression can become a lambda of 0 
    go (n, e) =
        go (n, ELam [] e)

toCpsC :: Expr S -> Val -> State CpsConv CExp
toCpsC t@ETerm{} c = do
    t' <- toVal t
    pure $ CFunCall c [t']

toCpsC e@ELam{} c = do
    e' <- toVal e
    pure $ CFunCall c [e']

-- funcalls can be on S if this is used
toCpsC (IfThenElse b t f) c = do
    t' <- toCpsC t c
    f' <- toCpsC f c
    toCps b $ \b' -> pure $ CIfThenElse b' t' f'

{-
--This generates an extra lambda, and requires CFunCall to call a Val, not an S
toCpsC (IfThenElse b t f) c = do
    k    <- genvar
    t'   <- toCpsC t (CVar k)
    f'   <- toCpsC f (CVar k)
    body <- toCps b $ \b' -> pure $ CIfThenElse b' t' f'
    pure $ CFunCall (CFunDef [k] body) [c] -- generates a lambda
    -- pure $ CLet k c body -- replace with let
-}

-- guess
toCpsC (ELet a b body) c =
    toCps b $ \b' ->
        CLet a b' <$> toCpsC body c

toCpsC (EBinPrimOp op a b) c =
    toCps a $ \a' ->
        toCps b $ \b' -> do
            bv   <- genvar
            rest <- toCpsC (ETerm (Var bv)) c
            pure $ CBinOp bv op a' b' rest

toCpsC (EUnPrimOp op a) c =
    toCps a $ \a' -> do
        bv   <- genvar
        rest <- toCpsC (ETerm (Var bv)) c
        pure $ CUnOp bv op a' rest


toCpsC (EApp f xs) c =
    toCps f $ \f' ->
        toCpss xs $ \xs' ->
            pure $ CFunCall f' (xs' ++ [c])

toCps :: Expr S -> (Val -> State CpsConv CExp) -> State CpsConv CExp

toCps t@ETerm{} k =
    k =<< toVal t

toCps e@ELam{} k =
    k =<< toVal e

toCps (EApp f xs) k = do

    -- Prepare the lambda
    rv      <- genvar
    krv     <- k (CVar rv)
    let lam = CFunDef [rv] krv

    toCps f $ \f' ->
        toCpss xs $ \xs' -> do
            l    <- genvar
            let fc = CFunCall f' (xs' ++ [CVar l])
            pure $ CLet l lam fc -- HERE
            -- stop putting the lambda straight into the list

-- guess
toCps (EBinPrimOp op a b) k =
    toCps a $ \a' ->
        toCps b $ \b' -> do
            bv <- genvar
            CBinOp bv op a' b' <$> k (CVar bv)

toCpss :: [Expr S] -> ([Val] -> State CpsConv CExp) -> State CpsConv CExp
toCpss     [] k = k []
toCpss (e:es) k =
    toCps e $ \e' ->
        toCpss es $ \es' ->
            k (e':es')

toVal :: Expr S -> State CpsConv Val

toVal (ETerm (Var v)) =
    pure $ CVar v

toVal (ETerm (LitInt i)) =
    pure $ CLitInt i

toVal (ETerm (LitBool t)) =
    pure $ CLitBool t

toVal (ETerm (LitString s)) =
    pure $ CLitString s

toVal (ELam vs body) = do

    -- Prepare the lambda
    k       <- genvar
    body'   <- toCpsC body (CVar k)
    let lam = CFunDef (vs ++ [k]) body'
    pure lam

asEnvParam :: Set S -> S
asEnvParam ps = mconcat ["env{", C8.intercalate "," $ S.toList ps, "}"]

genvar :: State CpsConv S
genvar = do
    CpsConv n ls <- get
    put $ CpsConv (n + 1) ls
    pure . C8.pack $ "c" ++ show n

liftToTopLevel :: Val -> State CpsConv Val
liftToTopLevel val = do
    tlname <- genvar
    CpsConv n ls <- get
    put $ CpsConv n ((tlname,val):ls)
    pure $ CVar tlname
