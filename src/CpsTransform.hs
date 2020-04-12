{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module CpsTransform where

import CpsTypes

import           Core (TopLevelEnv (..))
import qualified Core as L
import           CpsEval
import           State

import           Control.Monad               (zipWithM)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8

{- Plain top level functions are currently mixed into main,
   to support mutual recursion.
   Perhaps they could be exported separately, so that
   top-level expressions could refer to them  -}
cpsTransformTopLevel :: TopLevelEnv ByteString
                     -> ( [Val ByteString]
                        , [(ByteString, Cexp ByteString)]
                        , Cexp ByteString
                        , [DValue ByteString]
                        )
cpsTransformTopLevel (TopLevelEnv topLevelEnv) = do

    let (mainExpr, exprs, consts, funs) = categorise topLevelEnv

        (mainExpr', exprs') = fst . runState (do

                let (ons, oes) = unzip exprs
                exprs' <- mapM (\e -> go e (pure . CHalt)) oes

                let (mns, mes) = unzip funs
                mainExpr' <- go (L.EFix mns mes mainExpr) (pure . CHalt)

                pure (mainExpr', zip ons exprs')) $ 0

    let (vars, vals) = unzip consts
        vars' = map VVar vars
        vals' = map termToDVals vals

    (vars', exprs', mainExpr', vals')

termToDVals :: L.Expr s -> DValue s
termToDVals (L.ETerm    (L.LitInt i)) = DInt i
termToDVals (L.ETerm   (L.LitBool b)) = DBool b
termToDVals (L.ETerm (L.LitString s)) = DString s

categorise :: [(ByteString, L.Expr ByteString)]
           -> ( L.Expr ByteString
              , [(ByteString, L.Expr ByteString)]
              , [(ByteString, L.Expr ByteString)]
              , [(ByteString, L.Expr ByteString)]
              )
categorise = go Nothing [] [] []
   where
   go (Just mMain) exprs consts funs                                [] = (mMain, exprs, consts, funs)
   go            _ exprs consts funs           (("main", mainExpr):es) = go (Just mainExpr) exprs    consts    funs  es
   go        mMain exprs consts funs          (e@(_, (L.ELam _ _)):es) = go          mMain  exprs    consts (e:funs) es
   go        mMain exprs consts funs  (e@(_, L.ETerm (L.LitInt _)):es) = go          mMain  exprs (e:consts)   funs  es
   go        mMain exprs consts funs (e@(_, L.ETerm (L.LitBool _)):es) = go          mMain  exprs (e:consts)   funs  es

   go mMain exprs consts funs ((n, e):es) = go          mMain  ((n,e):exprs)    consts funs es

   go _ _ _ _ ((n,e):_) = error . C8.unpack $ "Can a top-level '" <> n <> "' be: " <> (C8.pack $ show e) <> " ?"

go :: L.Expr ByteString
   -> (Val ByteString -> State Int (Cexp ByteString))
   -> State Int (Cexp ByteString)

go (L.ETerm (L.Var v)) c = c (VVar v)

-- Treat DCons like var? or function call?
go (L.ETerm (L.DCons v)) c = c (VVar v)

go (L.ETerm (L.LitInt i)) c = c (VInt i)
go (L.ETerm (L.LitBool b)) c = c (VBool b)

go (L.ETerm (L.LitString s)) c = c (VString s)

go (L.ELam v e) c = do
    k      <- genvar
    lastly <- go e $ \e' ->
        pure $ CApp k [e']
    f      <- genvar
    cf     <- c f
    pure $ CFix [(f, [VVar v, k], lastly)] cf

go (L.EApp a b) c = do
    r   <- genvar
    goa <- go a $ \a' ->
             go b $ \b' ->
               pure $ CApp a' [b', r]
    x   <- genvar
    cvx <- c x
    pure $ CFix [(r, [x], cvx)] goa

-- Apply c to the result of a transformed Let expression
-- c (cps (Let x = y in z)
go (L.ELet x y z) c = do
    let lam = L.ELam x z
    let app = L.EApp lam y
    go app c

go (L.EFix funNames funBodies restOfProg) c = do
    functions'  <- zipWithM goFun funNames funBodies
    restOfProg' <- go restOfProg c
    pure $ CFix functions' restOfProg'

    where
    goFun fname (L.ELam fparam fbody) = do
        c'     <- genvar
        fbody' <- go fbody (\br -> pure $ CApp c' [br])
        {- Run the body,
           Apply c' to the result,
           where c' is a fresh cont parameter
           added to the function definition
        -}
        pure ( VVar fname
             , [ VVar fparam, c']
             , fbody'
             )

go (L.EUnPrimOp i e) c = do
    w  <- genvar
    cw <- c w
    go e $ \e' ->
        pure $ CPrimOp (fromUnOp i) [e'] [w] [cw]

go (L.EBinPrimOp op a b) c = do -- Guess
    w  <- genvar
    cw <- c w
    go a $ \a' ->
        go b $ \b' ->
            pure $ CPrimOp (fromBinOp op) [a', b'] [w] [cw]

go (L.IfThenElse p t f) c =
    go p $ \p' -> do
        t' <- go t c
        f' <- go f c
        pure $ CSwitch p' t' f'

go x _ = error $ "Non-exhaust go: " ++ show x

genvar :: State Int (Val ByteString)
genvar = do
    n <- get
    put (n + 1)
    pure . VVar $ "var" <> C8.pack (show n) 