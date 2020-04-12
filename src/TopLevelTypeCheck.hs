{-# LANGUAGE OverloadedStrings #-}

module TopLevelTypeCheck where

import Core
import TypeCheck
import TypeTypes

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List                   (partition)
import qualified Data.Map              as M

topLevelTypeCheck :: [(ByteString, Expr ByteString)] -> IO ()
topLevelTypeCheck defs = go M.empty defs 100

topLevelTypeCheck' :: [Defn ByteString] -> IO ()
topLevelTypeCheck' defs = do

    -- TODO check all DataDef in defs for typevariables same left as right

    let (tys, funs) =
          partition (\x -> case x of
                             DataDef{} -> True
                             FunDef{} -> False) defs

    let dcTypeEnv = M.fromList $ concatMap typify tys
    let fundefs = map (\(FunDef n e) -> (n, e)) funs
    go dcTypeEnv fundefs 100

    where
    typify (DataDef t@(TC tyname tyvars) dcs) = map typify' dcs
        where
        typify' (DataCons dcname b) = 
            (dcname, Forall tyvars (foldr TArr (TCon t) b))

go :: M.Map ByteString (Scheme ByteString) -> [(ByteString, Expr ByteString)] -> Int -> IO ()
go env                  [] _ = do putStrLn "Type-check complete"
                                  mapM_ putStrLn . map (\(n,e) -> C8.unpack n ++ ": " ++ show e)
                                                 . M.toList
                                                 $ env
go env ((name, expr):rest) n = do
    let env' = M.insert name (Forall [] $ TVar $ TV (C8.pack $ "_a_" ++ show n)) env -- guess for top-level recursion.  TODO unhardcode variable
    case inferExpr (TypeEnv env') expr of
        Right exprType ->
            go (M.insert name exprType env) rest (n + 1)
        Left err ->
            error $ "Could not infer type of: " <> C8.unpack name <> "\nError was: " <> show err
