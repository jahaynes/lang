{- Converts data-type declarations into funtions in the top-level environment, e.g.

    data List = Nil | Cons a List

        should become

    Nil       n c = n
    Cons x xs n c = c x xs
-}
module DataTypes (TopLevelEnv (..), desugarDataTypes) where

import           Control.Monad               (replicateM)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Monoid                 ((<>))

import Core
import State
import TypeTypes

desugarDataTypes :: TopLevel ByteString -> TopLevelEnv ByteString
desugarDataTypes topLevel = fst $ runState (desugarDataTypesState topLevel) 0

desugarDataTypesState :: TopLevel ByteString -> State Int (TopLevelEnv ByteString)
desugarDataTypesState (TopLevel defs) =
  TopLevelEnv . concat <$> mapM go defs
  where
  go (FunDef name expr) = pure [(name, expr)]
  go (DataDef tcon cs) = do
    sumVars   <- replicateM (length cs) (genSumVar tcon)
    prodVarss <- mapM genProdVars cs
    pure . map makeDataConsFun
         . zip [0..]
         . map (\(dc, pv) -> (dc, pv, sumVars))  --WARN: this re-use of sumVars might break alpha 
         $ prodVarss

    where
    makeDataConsFun :: (Int, (ByteString, [ByteString], [ByteString])) -> (ByteString, Expr ByteString)
    makeDataConsFun (nth, (dc, prodVars, sumVars)) =
        let params    = prodVars ++ sumVars
            nthSumVar = sumVars !! nth
            core      = ETerm (Var nthSumVar)
            -- rhs       = foldl EApp core (map (ETerm . Var) prodVars)
            -- body      = foldr ELam rhs params
            vars = map (ETerm . Var) prodVars
        in (dc, error "makeDataConsFun")

    -- Generates [n, c] from the top example
    genProdVars :: DataCons ByteString -> State Int (ByteString, [ByteString])
    genProdVars (DataCons a xs) = do
        ps <- replicateM (length xs) genProdVar
        pure (a, ps)
        where
        genProdVar = do
            n <- get
            put (n + 1)
            pure $ a <> i2_s n

    -- Generate [], [x, xs] from the example
    genSumVar :: TCon ByteString -> State Int ByteString
    genSumVar (TC tc _) = do -- Is the _ needed necessary?
        n <- get
        put (n + 1)
        pure $ tc <> i2_s n

    i2_s :: Int -> ByteString
    i2_s n = C8.pack ('_' : show n)
