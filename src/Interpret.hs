{-# LANGUAGE OverloadedStrings #-}

module Interpret where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.List             as L
import           Data.Monoid                 ((<>))

import Core
import PrettyExpr
import SourceMap    (SourceMap)

eval :: TopLevelEnv ByteString -> SourceMap ByteString -> Expr ByteString -> [Expr ByteString]
eval (TopLevelEnv topLevelEnv) sourceMap = loop
  where
  loop e =
    case step e of
      Just e' -> e : loop e'
      Nothing -> [e]

  step :: Expr ByteString -> Changed (Expr ByteString)

  step (EApp (ELam v dest) src) =
      changed $ subst v src dest

  step (ELet a b c) =
    changed $ subst a b c

  step (EApp e1 e2) =
    ((\e1' -> EApp e1' e2 ) <$> step e1) `orElse`
    ((\e2' -> EApp e1  e2') <$> step e2) `orElse`   --TODO is this line unnecessary?
    unchanged

  step (IfThenElse (ETerm (LitBool b)) t f) = if b then changed t else changed f
  step (IfThenElse p t f) = 
      ((\p' -> IfThenElse p' t f) <$> step p) `orElse`
      error ("Could not reduce to bool: " ++ show p)

  step (ETerm (Var v)) = 
    case L.lookup v topLevelEnv of
      Just tlExpr -> changed tlExpr
      Nothing     -> error $ show v ++ " not in scope!"

  step (ETerm (DCons dc)) = 
    case L.lookup dc topLevelEnv of
      Just tlExpr -> changed tlExpr
      Nothing     -> error $ show dc ++ " not in scope!"

  step (ETerm _) = unchanged --TODO not so catch-all

  step (EUnPrimOp Err (ETerm (LitString s))) = error $ concat ["RUNTIME_ERROR: ", C8.unpack s]

  step (EBinPrimOp EqI     (ETerm   (LitBool a))   (ETerm (LitBool b))) = changed $ ETerm   (LitBool $ a == b)

  step (EBinPrimOp AddI    (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm    (LitInt $  a + b)
  step (EBinPrimOp SubI    (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm    (LitInt $  a - b)
  step (EBinPrimOp MulI    (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm    (LitInt $  a * b)
  step (EBinPrimOp DivI    (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm    (LitInt $  a `div` b)
  step (EBinPrimOp ModI    (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm    (LitInt $  a `mod` b)
  
  step (EBinPrimOp LtI     (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm   (LitBool $ a <  b)
  step (EBinPrimOp LtEqI   (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm   (LitBool $ a <= b)
  step (EBinPrimOp GtI     (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm   (LitBool $ a >  b)
  step (EBinPrimOp GtEqI   (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm   (LitBool $ a >= b)
  step (EBinPrimOp EqI     (ETerm    (LitInt a))   (ETerm  (LitInt b))) = changed $ ETerm   (LitBool $ a == b)
  
  step (EBinPrimOp ConcatS (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitString $ a <> b)
  
  step (EBinPrimOp LtI     (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitBool   $ a <  b)
  step (EBinPrimOp LtEqI   (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitBool   $ a <= b)  
  step (EBinPrimOp GtI     (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitBool   $ a >  b)
  step (EBinPrimOp GtEqI   (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitBool   $ a >= b)  
  step (EBinPrimOp EqI     (ETerm (LitString a)) (ETerm (LitString b))) = changed $ ETerm (LitBool   $ a == b)

  step (EBinPrimOp op a b) =
    ((\a' -> EBinPrimOp op a' b ) <$> step a) `orElse`
    ((\b' -> EBinPrimOp op a  b') <$> step b) `orElse`
    error ("Could not run primop: " ++ show (op, a, b))

  step (EUnPrimOp EShow (ETerm   (LitBool b))) = changed . ETerm . LitString . C8.pack . show $ b
  step (EUnPrimOp EShow (ETerm    (LitInt n))) = changed . ETerm . LitString . C8.pack . show $ n
  step (EUnPrimOp EShow (ETerm (LitString s))) = changed . ETerm . LitString . C8.pack . show $ s -- TODO
  step (EUnPrimOp EShow e)                     = (EUnPrimOp EShow <$> step e)
    `orElse` (error $ "Could not reduce to showable: " ++ show (PrettyExpr sourceMap e))

  step e = error $ "Do not know how to reduce: " ++ show (PrettyExpr sourceMap e)

subst :: (Eq s, Show s) => s -> Expr s -> Expr s -> Expr s
-- x[x := N]    ≡ N
-- y[x := N]    ≡ y, if x ≠ y

subst x y e@(ETerm (Var v)) | x == v    = y
                            | otherwise = e

subst _ _ l@(ETerm _) = l

-- (M1 M2)[x := N]  ≡ (M1[x := N]) (M2[x := N])
subst x y (EApp a b) = EApp (subst x y a) (subst x y b)

-- Assuming works like app
subst x y (IfThenElse p t f) = IfThenElse (subst x y p) (subst x y t) (subst x y f)

subst x y (EUnPrimOp op e) = EUnPrimOp op (subst x y e)

-- Assuming works like app
subst x y (EBinPrimOp op a b) = EBinPrimOp op (subst x y a) (subst x y b)

subst x y l@(ELam v b)
    -- (λx.M)[x := N]   λx.M
    | x == v = l
    -- (λy.M)[x := N] ≡ λy.(M[x := N]), if x ≠ y, provided y ∉ FV(N)
    | otherwise = ELam v (subst x y b)

subst x y l@(ELet a b c)
    | x == a = l
    | otherwise = ELet a (subst x y b) (subst x y c)

type Changed a = Maybe a

changed :: a -> Changed a
changed = Just

orElse :: Changed a -> Changed a -> Changed a
orElse ca@(Just _)  _ = ca
orElse          _  cb = cb

unchanged :: Changed a
unchanged = Nothing
    
