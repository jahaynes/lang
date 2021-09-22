{-# LANGUAGE OverloadedStrings #-}

module CpsClosureConvert (closureConvert) where

import Core
import CpsLang
import CpsPrint
import State

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List                   (intercalate)
import           Data.Map                    (Map)
import qualified Data.Map as M
import           Data.Set                    (Set, (\\))
import qualified Data.Set as S
import           Text.Printf                 (printf)

data ConvertState =
    ConvertState { num           :: !Int
                 , topLevelNames :: !(Set S)
                 , lifted        :: ![(S, Val)]
                 } deriving Show

closureConvert :: [(S, Val)] -> IO ()
closureConvert tles = do

    let startTopLevels =
            S.fromList $ "halt" : map fst tles

    let (xs, ConvertState _ _ ys) =
            runState (mapM goTopLevel tles) (ConvertState 1 startTopLevels [])

    mapM_ render $ xs ++ ys

goTopLevel (n, CFunDef vs body) = do
    body' <- goExp body
    pure (n, CFunDef vs body')

goTopLevel i@(n, CLitInt _) = pure i

{-
-- very rough attempt at avoiding f(createClosure g {v})
-- (for just one var)
-- overzealus - should only extract closurecreates, not every val
goExp (CFunCall f [x]) = do
    f' <- goVal f
    x' <- goVal x
    g <- genvar
    pure $ CLet g x' (CFunCall f' [CVar g])
-}

goExp (CFunCall f xs) =
    CFunCall <$> goVal f <*> mapM goVal xs

{-
goExp (CLet a b c) = do
    b' <- goVal b
    c' <- goExp c
    case (b', c') of
        -- neaten closure
        -- let x = callclos f in x
        -- becomes callclos f
        (CCallClos cf xs, CFunCall (CVar f) ys) 
            | a == f -> do
                let e = CVar $ renderEnv (S.fromList xs) -- xs
                pure $ CFunCall (CVar cf) (e : ys)
        _ -> pure $ CLet a b' c'
-}

goExp (CLet a b c) =
    CLet a <$> goVal b <*> goExp c

goExp (CBinOp bv op a b rest) =
    CBinOp bv op <$> goVal a <*> goVal b <*> goExp rest

goExp (CUnOp bv op a rest) =
    CUnOp bv op <$> goVal a <*> goExp rest

goExp (CIfThenElse b t f) =
    CIfThenElse <$> goVal b <*> goExp t <*> goExp f

goExp e =
    error $ show ("goExp", e)

--non-toplevel lambda... name it promote to top-level
goVal :: Val -> State ConvertState Val
goVal (CFunDef vs body) = do

    -- Keep recursing
    body' <- goExp body
    let l = CFunDef vs body'

    -- Handle this level
    tlNames <- getTopLevelNames
    let free = getVariables l \\ tlNames
    if null free

        -- No free variables - can become a lambda
        then CVar <$> liftLambda l

        -- Some free variables - must become a closure
        else do
            
            let c = CFunDef (renderEnv free : vs) body'

            -- lift it
            g <- liftLambda c

            -- modify call site
            pure $ CCreateClos g (S.toList free)

goVal i@CLitInt{} =
    pure i

goVal b@CLitBool{} =
    pure b

goVal s@CLitString{} =
    pure s

goVal c@CVar{} =
    pure c

goVal e =
    error $ show ("goVal", e)

renderEnv free =
    "env{" <> (C8.intercalate "," $ S.toList free) <> "}"

liftLambda :: Val -> State ConvertState S
liftLambda val = do
    n <- genvar
    convertState <- get
    put $ convertState { lifted = (n, val) : lifted convertState }
    pure n

getTopLevelNames :: State ConvertState (Set S)
getTopLevelNames = do
    ConvertState _ xs ls <- get
    pure $ xs <> S.fromList (map fst ls)

genvar :: State ConvertState S
genvar = do
    ConvertState n xs ls <- get
    put $ ConvertState (n + 1) xs ls
    pure . C8.pack $ "g" ++ show n