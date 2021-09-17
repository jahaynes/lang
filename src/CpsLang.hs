{-# LANGUAGE OverloadedStrings #-}

module CpsLang where

import Core
import State

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List                   (intercalate)
import           Data.Set                    (Set, (\\))
import qualified Data.Set as S
import           Text.Printf                 (printf)

type S = ByteString

{-
    toCpsC uses a var as c, producing an App as the continuation itself
    toCps is given a k (a host-lang continuation which it applies)
-}

data CExp = CFunCall Val [Val] -- No bindingvar/rest with the aim to tailcall
          | CIfThenElse Val CExp CExp
          | CBinOp S BinOp Val Val CExp
          | CUnOp S UnOp Val CExp
          | CLet S Val CExp


data Val = CVar S
         | CLitInt Integer
         | CLitBool Bool
         | CLitString S
         | CFunDef [S] CExp
         | CCreateClos S [S]

class GetVariables a where
    getVariables :: a -> Set S

instance GetVariables Val where
    getVariables (CVar s)          = S.singleton s
    getVariables (CLitInt _)       = mempty
    getVariables (CLitBool _)      = mempty
    getVariables (CLitString _)    = mempty
    
    --If they're free in a sub-lambda, we need them outside in order to construct a closure,
    --so don't discard them.
    getVariables (CFunDef vs body) = getVariables body \\ S.fromList vs

    -- Unsure
    getVariables (CCreateClos f mkEnv) = S.insert f $ S.fromList mkEnv


instance GetVariables a => GetVariables [a] where
    getVariables xs = mconcat (map getVariables xs)

instance GetVariables CExp where

    getVariables (CFunCall f xs) =
        getVariables f <> getVariables xs

    getVariables (CIfThenElse b t f) =
        getVariables b <> getVariables t <> getVariables f

    getVariables (CBinOp bv _op a b rest) =
        getVariables a <> getVariables b <> (getVariables rest \\ S.singleton bv)

    getVariables (CLet a b c) =
        -- unsure whether a should be declared free if in b (recursion?)
        -- 'conversative' approach is to declare it for now I guess
        getVariables b <> (getVariables c \\ S.singleton a)

