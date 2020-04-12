{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alphabetise
import Core
import CpsEval
import CpsTransform
import DataTypes
import Interpret
import LambdaLift
import Lexer
import LexState
import Parser
import PrettyExpr
import PrettyTopLevel
import SourceMap
import Tokens
import TopLevelParser
import TopLevelTypeCheck

import CpsTypes

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

isVerbose :: [String] -> Bool
isVerbose ["-v"] = True
isVerbose      _ = False

main :: IO ()
main = do

    verbose <- isVerbose <$> getArgs

    i <- C8.getContents
    showRaw i

    let spTokens = lexer i
    C8.putStrLn " * Tokens (Positional information elided) *"
    print $ map snd spTokens

    let TopLevel defs = parse spTokens
    C8.putStrLn "\n * Top-level definitions *"
    mapM_ print defs

    let (defs', sourceMap@(SourceMap sm)) = alphabetise defs
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

    C8.putStrLn "\n * Lifting recursive lambdas *"
    liftedTle <- lambdaLiftTopLevel topLevelEnv

    C8.putStrLn "\n * Lambda lift resulted in (pretty) *"
    print (PrettyTopLevel sourceMap liftedTle)
    
    runWithBetaReduction verbose sourceMap liftedTle

    runWithCpsSemantics liftedTle

runWithCpsSemantics :: TopLevelEnv ByteString -> IO ()
runWithCpsSemantics topLevelEnv = do

    C8.putStrLn "\n * Will try to CPS *"
    let (vars, nonMainExprs, mainExpr, vals) = cpsTransformTopLevel topLevelEnv

    C8.putStrLn "\n * Transformed to: *"
    print ("main", mainExpr)
    mapM_ print nonMainExprs

    let (vars', vals') = unzip (map cpsEvaluateNonMainExpression nonMainExprs)
        vars''         = vars ++ vars'
        vals''         = vals ++ vals'
    C8.putStrLn "\n * Toplevels evaluated to: *"
    forM_ (zip vars'' vals'') $ \(VVar var, val) -> do
      C8.putStr $ var <> ": "
      print val

    C8.putStrLn "\n * Will try to Cps Eval: *"
    print $ cpsEval vars'' mainExpr vals'' undefined

    {- TODO: Toplevels which are not constants,
       functions or main can not yet refer to one another.

       Will need to build a call graph to resolve this.
     -}
    where
    cpsEvaluateNonMainExpression :: (ByteString, Cexp ByteString) -> (Val ByteString, DValue ByteString)
    cpsEvaluateNonMainExpression (name, expr) =
      let Answer val = cpsEval [] expr [] undefined
      in (VVar name, val)

runWithBetaReduction :: Bool -> SourceMap ByteString -> TopLevelEnv ByteString -> IO ()
runWithBetaReduction verbose sourceMap topLevelEnv = do

    let mainExpr = findMain topLevelEnv
    C8.putStrLn "\n * Main declaration located *"

    let evald  = eval topLevelEnv sourceMap mainExpr
        logs   = init evald
        result = last evald

    when verbose $ do
        C8.putStrLn "\n * Logs (Pretty) *"
        mapM_ (\l -> (putStrLn . show . PrettyExpr sourceMap $ l) >> putStrLn "") logs

    C8.putStrLn "\n * Evaluation *"
    print result

    C8.putStrLn "\n * Evaluation (Pretty) *"
    putStrLn . show $ PrettyExpr sourceMap result
