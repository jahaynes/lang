module Backends.JsPretty where

import CpsTypes
import Pretty
import SourceMap

import Data.ByteString.Char8 (ByteString, unpack)
import Data.List (intercalate)
import Text.Printf

data JsPretty =
    JsPretty (SourceMap ByteString) (Cexp ByteString) deriving Show

instance Pretty JsPretty where
    pretty n (JsPretty sm cfix@CFix{})       = prettyCFix n sm cfix
    pretty n (JsPretty sm cApp@CApp{})       = prettyCApp n sm cApp
    pretty n (JsPretty sm cPrimOp@CPrimOp{}) = prettyCPrimOp n sm cPrimOp
    pretty n (JsPretty sm cSwitch@CSwitch{}) = prettyCSwitch n sm cSwitch
    pretty n (JsPretty sm cHalt@CHalt{})     = prettyCHalt n sm cHalt

prettyCFix :: Int
           -> SourceMap ByteString
           -> Cexp ByteString
           -> (IsCompound, String)
prettyCFix n sm   (CFix    [] rest) = prettyCFix n sm rest
prettyCFix n sm x@(CFix defns rest) = do
    let defns' = unlines $ map (prettyDefn n sm) defns
    simple $ intercalate "\n" [ defns'
                              , paren $ pretty n (JsPretty sm rest)]
-- right place?
prettyCFix n sm cHalt@CHalt{} = prettyCHalt n sm cHalt
prettyCFix n sm cPrimOp@CPrimOp{} = prettyCPrimOp n sm cPrimOp

prettyDefn :: Int
           -> SourceMap ByteString
           -> (Val ByteString, [Val ByteString], Cexp ByteString)
           -> String
prettyDefn n sm (fname, args, body) = do
    let fname' = prettyVar sm fname
        args'  = map (prettyVar sm) args
    let topLine = printf "function %s(%s) {"
                         fname'
                         (intercalate ", " args')
    let body' = paren $ pretty (n+2) (JsPretty sm body)
    intercalate "\n" [ replicate n ' ' <> topLine
                     , body'
                     , replicate n ' ' <> "}"
                     ]

prettyCApp :: Int
           -> SourceMap ByteString
           -> Cexp ByteString
           -> (IsCompound, String)
prettyCApp n sm (CApp fun args) =
    let fun'  = prettyVar sm fun
        args' = intercalate ", " $ map (prettyVar sm) args
    in simple $ replicate n ' ' <> printf "%s(%s)" fun' args' <> ";"

prettyCPrimOp :: Int
              -> SourceMap ByteString
              -> Cexp ByteString
              -> (IsCompound, String)
prettyCPrimOp n sm (CPrimOp op [a, b] [d] [k]) =

    let op' = prettyOp op
        d'  = prettyVar sm d
        a'  = prettyVar sm a
        b'  = prettyVar sm b
        ls = intercalate "\n" [ replicate n ' ' <> unwords ["var", d', "=", a', op', b' <> ";"]
                              , paren $ pretty n (JsPretty sm k)
                              ]
    in simple ls

prettyCPrimOp n sm (CPrimOp op [a] [d] [k]) =

    let op' = prettyOp op
        d'  = prettyVar sm d
        a'  = prettyVar sm a
        ls  = intercalate "\n" [ replicate n ' ' <> unwords ["var", d', "=", op', a' <> ";"]
                               , paren $ pretty n (JsPretty sm k)
                               ]
    in simple ls

prettyCPrimOp n _ x = error $ show ("prettyCPrimOp", n, x)

prettyCSwitch :: Int -> SourceMap ByteString -> Cexp ByteString -> (IsCompound, String)
prettyCSwitch n sm (CSwitch b tk fk) =

    let tk' = paren $ pretty (n+2) (JsPretty sm tk)
        fk' = paren $ pretty (n+2) (JsPretty sm fk)
    in simple $ intercalate "\n" [ replicate n ' ' <> printf "if (%s) {" (prettyVar sm b)
                                 , tk'
                                 , replicate n ' ' <> "} else {"
                                 , fk'
                                 , replicate n ' ' <> "}"
                                 ]

prettyCHalt :: Int -> SourceMap ByteString -> Cexp ByteString -> (IsCompound, String)
prettyCHalt n sm (CHalt c) = do
    let c'   = prettyVar sm c
        ret  = replicate n ' ' <> printf "console.log(%s);" c'
--        halt = replicate n ' ' <> "// halt;"
    simple (intercalate "\n" [ret]) -- , halt])

prettyOp :: COp -> String
prettyOp GtI     = ">"
prettyOp AddI    = "+"
prettyOp SubI    = "-"
prettyOp MulI    = "*"
prettyOp DivI    = "/"
prettyOp ModI    = "%"
prettyOp EqI     = "=="
prettyOp LtI     = "<"
prettyOp LtEqI   = "<="
prettyOp GtEqI   = ">="
prettyOp EShow   = "show"
prettyOp ConcatS = "+"
prettyOp x       = error $ show ("OP", x)

prettyVar :: SourceMap ByteString -> Val ByteString -> String
prettyVar  _ (VBool True)  = "true"
prettyVar  _ (VBool False) = "false"
prettyVar  _ (VInt n)      = show n
prettyVar  _ (VString s)   = show s
prettyVar sm (VVar v)      = unpack $ lookupDef v sm
prettyVar  _        x      = error $ show ("prettyVar", x)
