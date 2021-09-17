{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alphabetise
import Cleanup
import Core
import CpsClosureConvert
import CpsTransform
import CpsPrint
import DataTypes
import Interpret
import Lexer
import LexState
import Parser
import PrettyExpr
import PrettyTopLevel
import SourceMap
import Tokens
import TopLevelParser

import           Control.Monad               (forM_, when)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Monoid                 ((<>))
import           System.Environment          (getArgs)

showRaw :: ByteString -> IO ()
showRaw raw = do
  C8.putStrLn "Raw:"
  C8.putStrLn raw

lexer :: ByteString -> [(SourcePos, Token ByteString)]
lexer raw =
  case runLexer raw of
    Left x -> error $ "Lexing failed: " ++ show x
    Right (LexState _ "", spTokens) -> spTokens
    Right (LexState _ lo,        _) -> error $ "Leftover tokens: " ++ C8.unpack lo

parse :: (Eq s, Show s) => [(SourcePos, Token s)] -> TopLevel s
parse spTokens =
  case runParser parseTopLevel spTokens of
    Left x              -> error $ "Parsing failed: " ++ show x
    Right ([], (_, ts)) -> ts
    Right (lo,       _) -> error $ "Leftover tokens: " ++ show lo

findMain :: TopLevelEnv ByteString -> Expr ByteString
findMain (TopLevelEnv topLevelEnv) =
  case L.lookup "main" topLevelEnv of
    Just expr -> expr
    Nothing   -> error $ "No main definition found"

main :: IO ()
main = do

    i <- C8.getContents
    showRaw i

    let spTokens = lexer i
    C8.putStrLn " * Tokens (Positional information elided) *"
    print $ map snd spTokens

    let (TopLevel defs) = parse spTokens
    C8.putStrLn "\n * Top-level definitions *"
    mapM_ print defs

    let cleanedUp = map cleanup defs

    let (defs', sourceMap@(SourceMap sm)) = alphabetise cleanedUp
    C8.putStrLn "\n * Top-level definitions (alphabetised) *"
    mapM_ print defs'

    C8.putStrLn "\n * Source map *"
    mapM_ print $ M.toList sm

    --C8.putStrLn "\n * About to type-check *"
    --topLevelTypeCheck' defs'

    let topLevelEnv@(TopLevelEnv tle) = desugarDataTypes (TopLevel defs')
    C8.putStrLn "\n * Top-level environment with data constructors *"
    mapM_ putStrLn $ map (\(k,v) -> show k <> ": " <> show v) tle

    C8.putStrLn "\n * Top-level environment with data constructors (pretty) *"
    print (PrettyTopLevel sourceMap topLevelEnv)

    let cps = cpsLangify topLevelEnv
    C8.putStrLn "\n * Cps transform applied *"
    mapM_ render cps

    C8.putStrLn "\n * Converting closures *"
    closureConvert cps

    runWithBetaReduction sourceMap topLevelEnv

runWithBetaReduction :: SourceMap ByteString -> TopLevelEnv ByteString -> IO ()
runWithBetaReduction sourceMap topLevelEnv = do

    let mainExpr = findMain topLevelEnv
    C8.putStrLn "\n * Main declaration located *"

    let evald  = eval topLevelEnv sourceMap mainExpr
        logs   = init evald
        result = last evald

    -- verbose
    when True $ do
        C8.putStrLn "\n * Logs (Pretty) *"
        mapM_ (\l -> (putStrLn . show . PrettyExpr sourceMap $ l) >> putStrLn "") logs

    C8.putStrLn "\n * Evaluation *"
    print result

    C8.putStrLn "\n * Evaluation (Pretty) *"
    putStrLn . show $ PrettyExpr sourceMap result
