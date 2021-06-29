module Core where

import TypeTypes

newtype TopLevelEnv s = TopLevelEnv [(s, Expr s)]
    deriving Show

newtype TopLevel s = TopLevel [Defn s]
    deriving Show



data Defn s = FunDef s (Expr s)
            | DataDef (TCon s) [DataCons s]
    deriving Show

data DataCons s = DataCons s [Type s]
    deriving Show



data Expr s = ETerm (Term s)
            | ELam s (Expr s)
            | EApp (Expr s) (Expr s)
            | ELet s (Expr s) (Expr s)
            | EFix [s] [Expr s] (Expr s)
            | EUnPrimOp UnOp (Expr s)
            | EBinPrimOp BinOp (Expr s) (Expr s)
            | IfThenElse (Expr s) (Expr s) (Expr s)
                deriving Show

data UnOp = EShow
          | Err
              deriving Show

data BinOp = AddI
           | SubI
           | MulI
           | DivI
           | ModI
           | EqI

           | LtEqI
           | LtI
           | GtEqI
           | GtI
           
           | ConcatS
               deriving Show

data Term s = Var s             -- Var gets subst.  Check others when added.
            | DCons s           -- unsure if this is the right place.  also, subst?
            | LitInt Integer
            | LitBool Bool     
            | LitString s
                deriving Show
