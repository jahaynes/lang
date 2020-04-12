module LambdaLift (lambdaLiftTopLevel) where

import Core
import State

import           Control.Monad      (forM)
import           Data.Map           (Map)
import qualified Data.Map      as M
import           Data.Set           (Set)
import qualified Data.Set      as S

data FreeInfo s =
    FreeInfo { vars :: Set s
             , recu :: Bool
             } deriving Show

-- Do calls to top-level count as free?

lambdaLiftTopLevel :: (Ord s, Show s) => TopLevelEnv s -> IO (TopLevelEnv s)
lambdaLiftTopLevel (TopLevelEnv tle) = TopLevelEnv
                                     . concat
                                   <$> mapM lambdaLiftExpression tle

    where
    lambdaLiftExpression :: (Ord s, Show s) => (s, Expr s) -> IO [(s, Expr s)]
    lambdaLiftExpression (name, expr) = concat <$> do
        print ("Looking at: ", name)
        case filter (recu . snd)
           . M.toList
           . snd
           $ runState (findLetsAndFreeVars expr) M.empty of
               [] -> do putStrLn "    No nested recursions\n"
                        pure [[(name, expr)]]
               letsAndFreeVars -> do
                   putStrLn $ "    Lifting out: " ++ show (length letsAndFreeVars) ++ " definitions\n"
                   forM letsAndFreeVars $ \(n, FreeInfo vs _) -> do
                       let (e, Just x) = runState (lambdaLift (S.toList vs) n expr) Nothing
                       pure [(name, e), (n, x)]

lambdaLift :: Eq s => [s] -> s -> Expr s -> State (Maybe (Expr s)) (Expr s)
lambdaLift extraVars yankee = go
    where
    go t@(ETerm _)         = pure t

    go (ELam v body)       = ELam v <$> go body

    go (EApp a@(ETerm (Var v)) b)
        | v == yankee = do
            a' <- go a
            b' <- go b
            -- This is the call-site modification to call f(free,x,y) instead of f(x,y)
            let a'' = foldl EApp a' (map (ETerm . Var) extraVars)
            pure $ EApp a'' b'

    go (EApp a b) = EApp <$> go a <*> go b

    go (ELet a b c) = do
        b' <- go b
        c' <- go c 
        if a == yankee
            then do 
                    -- This is the function def modification
                    -- e.g.        {f free a b =}
                    -- instead of  {f      a b =}
                    let b'' = foldr ELam b' extraVars

                    -- store the body for use as a new top level function
                    put (Just b'')

                    -- return this expression without the let a = b in ...
                    pure c'
            else pure $ ELet a b' c'

    go (EFix _ _ _)        = error "EFix even used any more?"
    go (IfThenElse p t f)  = IfThenElse <$> go p <*> go t <*> go f
    go (EUnPrimOp op a)    = EUnPrimOp op <$> go a
    go (EBinPrimOp op a b) = EBinPrimOp op <$> go a <*> go b

findLetsAndFreeVars :: (Ord s, Show s) => Expr s -> State (Map s (FreeInfo s)) (Set s)
findLetsAndFreeVars = go
    where
    go (ETerm t) =
        case t of
            Var v -> pure $ S.singleton v
            _     -> pure mempty

    go (ELam v body)         = S.delete v <$> go body

    go (EApp a b)            = mconcat <$> mapM go [a, b]

    go (ELet a b c)          = do underB <- go b
                                  underC <- go c
                                  let recursive = S.member a underB
                                      free      = S.delete a (underB <> underC)
                                  modify' (M.insert a (FreeInfo free recursive))
                                  pure free

    go (EFix _ _ _)          = error "EFix even used any more?"
    go (IfThenElse p t f)    = mconcat <$> mapM go [p, t, f]
    go (EUnPrimOp _ a)       = go a
    go (EBinPrimOp _ a b)    = mconcat <$> mapM go [a, b]
