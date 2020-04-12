module TypeCheck where

import Core
import TypeTypes

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Map as M

-- | Inference monad
type Infer s a = (ReaderT
                  (Env s)         -- Typing environment
                  (StateT         -- Inference state
                  InferState
                  (Except         -- Inference errors
                    (TypeError s)))
                  a)              -- Result

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 1 }  -- leave 0 for "own name"

type Constraint s = (Type s, Type s)

type Unifier s = (Subst s, [Constraint s])

data TypeError s
  = UnificationFail (Type s) (Type s)
  | InfiniteType (TVar s) (Type s)
  | UnboundVariable s
  | Ambigious [Constraint s]
  | UnificationMismatch [Type s] [Type s]
    deriving Show

-- | Constraint solver monad
type Solve s a = ExceptT (TypeError s) Identity a

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: (Eq s, FromString s, Ord s, Show s) => Env s -> Expr s -> Either (TypeError s) (Scheme s)
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> do error . unlines $ [ show err, show cs ]
                       Left err
        Right subst -> Right $ closeOver $ applyType subst ty

  where
  -- | Run the inference monad
  runInfer :: Env s -> Infer s (Type s, [Constraint s]) -> Either (TypeError s) (Type s, [Constraint s])
  runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

infer :: (FromString s, Ord s, Show s) => Expr s -> Infer s (Type s, [Constraint s])
infer expr = case expr of
  ETerm (LitInt _)    -> return (typeInt, [])
  ETerm (LitBool _)   -> return (typeBool, [])
  ETerm (LitString _) -> return (typeString, [])

  -- Treat dataconstructors as their church-encoded function names for now
  -- TODO: compare actual types here
  ETerm (DCons dc) -> do
      t <- lookupEnv dc
      return (t, [])

  ETerm (Var x) -> do 
      t <- lookupEnv x
      return (t, [])

  ELam v b -> do
      tv <- fresh
      (t, c) <- inEnv (v, Forall [] tv) (infer b)
      return (tv `TArr` t, c)

  EApp a b -> do
    (t1, c1) <- infer a
    (t2, c2) <- infer b
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])

  ELet x e1 e2 -> do
        env <- ask
        -- (t1, c1) <- infer e1 -- original
        tv <- fresh
        (t1, c1) <- inEnv (x, Forall [] tv) (infer e1) -- Hope this OK
        case runSolve c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (applyEnv sub env) (applyType sub t1)
                (t2, c2) <- inEnv (x, sc) $ local (applyEnv sub) (infer e2)
                return (t2, c1 ++ c2)

  EUnPrimOp Err errMsg -> do 

      -- Err can be any type
      t <- fresh

      -- Get the type of errMsg (and any constraints it produced)
      (t1, c1) <- infer errMsg

      -- Assert that errMsg's type is String
      let c = (t1, typeString)

      pure (t, c:c1)

  EUnPrimOp EShow e -> do

      -- Get e's type and constraints
      (_, c) <- infer e

      -- Ignore type, become string!
      pure (typeString, c)

  EBinPrimOp op e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = t1 `TArr` (t2 `TArr` tv)
        u2 = ops op
    return (tv, c1 ++ c2 ++ [(u1, u2)])

  IfThenElse p t f -> do
    (tp, cp) <- infer p
    (tt, ct) <- infer t
    (tf, cf) <- infer f
    let cs = (tp, typeBool)      -- The predicate must be bool
           : (tt, tf)            -- The true and false branches must be the same
           : concat [cp, ct, cf] -- Include any constraints produced in additional steps
    pure (tt, cs)

  a -> error $ "INFER: " ++ show a

applyEnv :: (Ord s, Show s) => Subst s -> Env s -> Env s
applyEnv s (TypeEnv env) = TypeEnv $ M.map (applyScheme s) env

applyScheme :: (Ord s, Show s) => Subst s -> Scheme s -> Scheme s
applyScheme (Subst s) (Forall as t)   = Forall as $ applyType s' t
                            where s' = Subst $ foldr M.delete s as

ops :: FromString s => BinOp -> Type s
ops AddI    = typeInt `TArr` (typeInt `TArr` typeInt)
ops SubI    = typeInt `TArr` (typeInt `TArr` typeInt)
ops MulI    = typeInt `TArr` (typeInt `TArr` typeInt)

ops EqI     = typeInt `TArr` (typeInt `TArr` typeBool)

ops LtEqI   = typeInt `TArr` (typeInt `TArr` typeBool)
ops LtI     = typeInt `TArr` (typeInt `TArr` typeBool)
ops GtEqI   = typeInt `TArr` (typeInt `TArr` typeBool)
ops GtI     = typeInt `TArr` (typeInt `TArr` typeBool)

ops ConcatS = typeString `TArr` (typeString `TArr` typeString)

-- | Extend type environment
inEnv :: (Ord s, Show s) => (s, Scheme s) -> Infer s a -> Infer s a
inEnv (x, sc) m = do
  let scope e = (remove e x) `extend` (x, sc)
  local scope m

extend :: (Ord s, Show s) => Env s -> (s, Scheme s) -> Env s
extend env (x, s) = env { types = M.insert x s (types env) }

remove :: (Ord s, Show s) => Env s -> s -> Env s
remove (TypeEnv env) var = TypeEnv (M.delete var env)


-- | Lookup type in the environment
lookupEnv :: (FromString s, Ord s, Show s) => s -> Infer s (Type s)
lookupEnv v = do

  TypeEnv env <- ask

  case M.lookup v env of

      Nothing -> throwError $ UnboundVariable v

      Just s  -> instantiate s

instantiate :: (FromString s, Ord s, Show s) => Scheme s -> Infer s (Type s)
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ M.fromList $ zip as as'
    return $ applyType s t

fresh :: FromString s => Infer s (Type s)
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

applyType :: (Ord s, Show s) => Subst s -> Type s -> Type s
applyType _ (TCon a)       = TCon a
applyType (Subst s) t@(TVar a) = M.findWithDefault t a s
applyType s (t1 `TArr` t2) = applyType s t1 `TArr` applyType s t2

applyTypeList :: (Ord s, Show s) => Subst s -> [Type s] -> [Type s]
applyTypeList = map . applyType

applyConstraint :: (Ord s, Show s) => Subst s -> Constraint s -> Constraint s
applyConstraint s (t1, t2) = (applyType s t1, applyType s t2)

applyConstraintList :: (Ord s, Show s) => Subst s -> [Constraint s] -> [Constraint s]
applyConstraintList = map . applyConstraint

-- | Run the constraint solver
runSolve :: (Eq s, Ord s, Show s) => [Constraint s] -> Either (TypeError s) (Subst s)
runSolve cs = runIdentity $ runExceptT $ solver st
  where st = (Subst M.empty, cs)

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: (Eq s, FromString s, Ord s, Show s) => Type s -> Scheme s
closeOver = normalize . generalize (TypeEnv M.empty)

generalize :: (Ord s, Show s) => Env s -> Type s -> Scheme s
generalize env t  = Forall as t
    where as = S.toList $ ftvType t `S.difference` ftvEnv env

-- Unification solver
solver :: (Eq s, Ord s, Show s) => Unifier s -> Solve s (Subst s)
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (su1 `compose` su, applyConstraintList su1 cs0)

ftvScheme :: (Ord s, Show s) => Scheme s -> S.Set (TVar s)
ftvScheme (Forall as t) = ftvType t `S.difference` S.fromList as

ftvSchemeList :: (Ord s, Show s) => [Scheme s] -> S.Set (TVar s)
ftvSchemeList = foldr (S.union . ftvScheme) S.empty

ftvEnv :: (Ord s, Show s) => Env s -> S.Set (TVar s)
ftvEnv (TypeEnv env) = ftvSchemeList $ M.elems env

ftvType :: (Ord s, Show s) => Type s -> S.Set (TVar s)
ftvType TCon{}         = S.empty
ftvType (TVar a)       = S.singleton a
ftvType (t1 `TArr` t2) = ftvType t1 `S.union` ftvType t2

unifies :: (Eq s, Ord s, Show s) => Type s -> Type s -> Solve s (Subst s)
unifies t1 t2 | t1 == t2 = return $ Subst M.empty
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]

unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: (Eq s, Ord s, Show s) => [Type s] -> [Type s] -> Solve s (Subst s)
unifyMany [] [] = return $ Subst M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (applyTypeList su1 ts1) (applyTypeList su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: (Ord s, Show s) => Subst s -> Subst s -> Subst s
(Subst s1) `compose` (Subst s2) = Subst $ M.map (applyType (Subst s1)) s2 `M.union` s1

bind :: (Eq s, Ord s, Show s) => TVar s -> Type s -> Solve s (Subst s)
bind a t | t == TVar a     = return $ Subst M.empty
         | occursCheckType a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ M.singleton a t)

occursCheckType :: (Ord s, Show s) => TVar s -> Type s -> Bool
occursCheckType a t = a `S.member` ftvType t

normalize :: (Eq s, FromString s, Show s) => Scheme s -> Scheme s
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"

letters :: FromString s => [s]
letters = fromString <$> ([1..] >>= flip replicateM ['a'..'z'])