{-# LANGUAGE OverloadedStrings #-}

module CpsTypes where

import qualified Core as L

data Val s = VVar s
           | VInt Integer
           | VBool Bool
           | VString s
             deriving (Eq, Show)

data Cexp s = CApp (Val s) [Val s]

            | CFix [            -- List of
                     ( Val s    --   fun name
                     , [Val s]  --   fun params
                     , Cexp s)  --   fun body
                   ] (Cexp s)   -- rest of program

            | CSwitch (Val s) (Cexp s) (Cexp s)

            | CPrimOp COp [Val s] [Val s] [Cexp s]

            | CHalt (Val s)            -- Guess
              deriving Show

data COp = EShow
         | Err
         | AddI
         | SubI
         | MulI
         | EqI
         | LtEqI
         | LtI
         | GtEqI
         | GtI
         | ConcatS
           deriving Show

fromBinOp :: L.BinOp -> COp
fromBinOp L.EqI     = EqI
fromBinOp L.AddI    = AddI
fromBinOp L.SubI    = SubI
fromBinOp L.MulI    = MulI

fromBinOp L.LtEqI   = LtEqI
fromBinOp L.LtI     = LtI
fromBinOp L.GtEqI   = GtEqI
fromBinOp L.GtI     = GtI

fromBinOp L.ConcatS = ConcatS
fromBinOp binOp = error $ show binOp

fromUnOp :: L.UnOp -> COp
fromUnOp L.Err = Err
fromUnOp L.EShow = EShow
