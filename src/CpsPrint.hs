{-# LANGUAGE OverloadedStrings #-}

module CpsPrint where

import Core
import CpsLang
import State

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List                   (intercalate)
import           Data.Set                    (Set, (\\))
import qualified Data.Set as S
import           Text.Printf                 (printf)

instance Show Val where
    show (CVar s)              = C8.unpack s
    show (CLitInt i)           = show i
    show (CLitBool b)          = show b
    show (CLitString s)        = show s
    show (CFunDef vs body)     = printf "(\\%s -> %s)" (showArgs vs) (show body)
    show (CCreateClos f mkEnv) = printf "createClos %s {%s}" (C8.unpack f) (showArgs mkEnv)

showArgs :: [S] -> String
showArgs = C8.unpack . C8.intercalate ","

showArgs' :: [Val] -> String
showArgs' vs = intercalate "," $ map show vs

instance Show CExp where
    show (CFunCall f xs)         = printf "%s(%s)" (show f) (showArgs' xs)
    show (CIfThenElse b t f)     = printf "if (%s) {%s} else {%s}" (show b) (show t) (show f)
    show (CBinOp bv op a b rest) = printf "letbo %s = %s %s %s in %s" (C8.unpack bv) (show a) (show op) (show b) (show rest)
    show (CUnOp bv op a rest)    = printf "letuo %s = %s %s in %s" (C8.unpack bv) (show op) (show a) (show rest)
    show (CLet a b c)            = printf "letl %s = %s in %s" (C8.unpack a) (show b) (show c)

render (n, CFunDef vs body) = do
    printf "%s(%s) {\n" (C8.unpack n) (showArgs vs)
    printf "  %s\n" (show body)
    putStrLn "}\n"

render (n, e) = do
    printf "%s() {\n" (C8.unpack n)
    printf "  %s\n" (show e)
    putStrLn "}\n"