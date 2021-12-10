module PrettyExpr where

import           Core
import           Pretty
import           PrettyOp
import           SourceMap (SourceMap)
import qualified SourceMap             as SM

import           Data.ByteString.Char8 (ByteString, unpack)

data PrettyExpr =
    PrettyExpr (SourceMap ByteString) (Expr ByteString)

instance Show PrettyExpr where
    show = paren . pretty 0

instance Pretty PrettyExpr where
    pretty _ (PrettyExpr sm (ETerm (      Var v))) = simple $ let v' = SM.lookupDef v sm in unpack v'
    pretty _ (PrettyExpr _  (ETerm (   LitInt i))) = simple $ show i
    pretty _ (PrettyExpr _  (ETerm (  LitBool b))) = simple $ show b
    pretty _ (PrettyExpr _  (ETerm (LitString s))) = simple $ mconcat ["\"", unpack s, "\""]

    pretty _ (PrettyExpr _  (ETerm (DCons dc))) = simple $ unpack dc

    pretty _ (PrettyExpr sm (ELet a b c)) =
        let a' = unpack $ SM.lookupDef a sm
            b' = paren . pretty 0 . PrettyExpr sm $ b
            c' = paren . pretty 0 . PrettyExpr sm $ c
        in compound ["let", a', "=", b', "in", c']

    pretty _ (PrettyExpr sm (ELam vs body)) = let vs'  = map (\v -> SM.lookupDef v sm) vs
                                                  lam' = unwords [ "\\" <> unwords (map unpack vs') <> "." 
                                                                 , paren . pretty 0 . PrettyExpr sm $ body
                                                                 ]
                                             in simple $ concat ["(", lam', ")"]
    pretty _ (PrettyExpr sm (EApp a bs)) = let (a':bs') = map (paren . pretty 0 . PrettyExpr sm) (a:bs)
                                           in compound (a':bs')

    pretty _ (PrettyExpr sm (EUnPrimOp op e)) = let op' = paren . pretty 0 $ PrettyUnOp op
                                                    e'  = paren . pretty 0 . PrettyExpr sm $ e
                                                   in compound [op', e']

    pretty _ (PrettyExpr sm (EBinPrimOp op a b)) = let op'      = paren . pretty 0 $ PrettyBinOp op
                                                       [a', b'] = map (paren . pretty 0 . PrettyExpr sm) [a, b]
                                                   in compound [op', a', b']

    pretty _ (PrettyExpr sm (IfThenElse p t f)) = let [p', t', f'] = map (paren . pretty 0 . PrettyExpr sm) [p, t, f]
                                                  in compound ["if", p', "then", t', "else", f']

