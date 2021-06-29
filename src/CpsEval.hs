module CpsEval ( Answer (..)
               , DValue (..)
               , cpsEval
               ) where

import CpsTypes

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Word                   (Word64)

type S = ByteString

data Loc = Loc Word64 deriving Eq

data Store s = Store Loc
                     (Loc -> DValue s)
                     (Loc -> Int)

data Answer s = Answer (DValue s) deriving Show

data DValue s = DInt Integer
              | DBool Bool
              | DString s
              | Func ([DValue s] -> Store s -> Answer s)

instance Show s => Show (DValue s) where
    show (DInt i)    = "DInt " ++ show i
    show (DBool b)   = "DBool " ++ show b
    show (DString s) = "DString " ++ show s
    show (Func _)    = "DValueFunc"

type Env s = Val s -> DValue s

-- Vee turns a Cps Value into a Denotable Value
-- Constants are left as is
-- Variables are looked up from the environment
vee :: Env s -> Val s -> DValue s
vee   _   (VInt i)    = DInt i
vee   _   (VBool b)   = DBool b
vee   _   (VString s) = DString s
vee env v@(VVar _)    = env v

bindn :: Env S -> [Val S] -> [DValue S] -> Env S
bindn env     []     [] = env
bindn env (v:vs) (d:ds) = bindn (bind env v d) vs ds

    where
    bind :: Env S -> Val S -> DValue S -> Env S
    bind env v@(VVar _) d w
        | v == w    = d
        | otherwise = env w

env0 :: Env ByteString
env0 = \k -> error $ unwords [ "Could not find"
                             , show k
                             , "in env"
                             ]

-- Yields answer by evaluating cexp in env where vars are bound to vals
cpsEval :: [Val S]    -- list of CPS vars
        -> Cexp S     -- a continuation expr
        -> [DValue S] -- list of denotable vals
        -> Answer S
cpsEval vs expr ds =

    -- Prepare an empty store
    let store = undefined

    -- Prepare an environment by binding vars to values
        env' = bindn env0 vs ds

    in ee expr env' store

--p34. Ee takes the denotation of a CPS expr
ee :: Cexp S
   -> Env S
   -> Store S
   -> Answer S
ee (CApp funName funArgs) env store =

        -- Resolve function name to its value
    let Func f = vee env funName

        -- Follow each variable name to its value
        args = map (vee env) funArgs

        -- Apply function to arguments
    in f args store

ee (CFix fl e) env store = ee e (g env) store

    where
    g :: Env S
      -> Env S
    g env' = do
        let funNames = map (\(n,_,_) -> n) fl
            funVals  = map (h env') fl
        bindn env' funNames funVals

    h :: Env S
      -> (Val S, [Val S], Cexp S)
      -> DValue S
    h env'' (_, vals, b) = Func $ \args -> ee b (bindn (g env'') vals args)

ee (CPrimOp op args argDests ks) env store =

    let argValues = map (vee env) args

        continuations = map evaluateContinuation ks

    in evalprim op
                argValues
                continuations
                store

    where
    evaluateContinuation :: Cexp S
                         -> [DValue S]
                         -> Store S
                         -> Answer S
    evaluateContinuation k vs store' =

        let env' = bindn env argDests vs

        in ee k env' store'

-- Lookup a boolean b, then evaluate the true- or false-continuation
ee (CSwitch b t f) env store =
    case vee env b of
        DBool True  -> ee t env store
        DBool False -> ee f env store
        other       -> error "Unexpected switch on nonbool"

-- Lookup the return name and return its value
ee (CHalt val) env store =
    case vee env val of
        a@(DInt _)    -> Answer a
        a@(DBool _)   -> Answer a
        a@(DString _) -> Answer a
        Func _        -> error "Tried to return a function at Halt"

evalprim :: COp                                 -- Operator
         -> [DValue S]                          -- Parameters
         -> [[DValue S] -> Store S -> Answer S] -- Continuations
         -> Store S                             -- Store
         -> Answer S                            -- Answer

    --Arithmetic
evalprim AddI [DInt i, DInt j] [c] = overflow (\() -> i + j) c
evalprim SubI [DInt i, DInt j] [c] = overflow (\() -> i - j) c
evalprim MulI [DInt i, DInt j] [c] = overflow (\() -> i * j) c
evalprim DivI [DInt i, DInt j] [c] = overflow (\() -> i `div` j) c
evalprim ModI [DInt i, DInt j] [c] = overflow (\() -> i `mod` j) c

    -- Comparisons
evalprim LtEqI [a, b] [c] = c [lteqi a b]
evalprim LtI   [a, b] [c] = c [lti   a b]
evalprim GtEqI [a, b] [c] = c [gteqi a b]
evalprim GtI   [a, b] [c] = c [gti   a b]
evalprim EqI   [a, b] [c] = c [equ   a b]

    -- String Concatenation
evalprim ConcatS [DString a, DString b] [c] = c [DString $ a <> b]

    -- Error & ToString
evalprim EShow    [DInt i] [c] = c [DString . C8.pack . show $ i]
evalprim EShow   [DBool b] [c] = c [DString . C8.pack . show $ b]
evalprim EShow [DString s] [c] = c [DString . C8.pack . show $ s]
evalprim Err   [DString e]   _ = error $ "ERROR: " ++ show e

evalprim a b _ = error $ show (a,b)

lteqi :: DValue S -> DValue S -> DValue S
lteqi    (DInt a)    (DInt b) = DBool (a <= b)
lteqi (DString a) (DString b) = DBool (a <= b)

lti :: DValue S -> DValue S -> DValue S
lti    (DInt a)    (DInt b) = DBool (a < b)
lti (DString a) (DString b) = DBool (a < b)

gteqi :: DValue S -> DValue S -> DValue S
gteqi    (DInt a)    (DInt b) = DBool (a >= b)
gteqi (DString a) (DString b) = DBool (a >= b)

gti :: DValue S -> DValue S -> DValue S
gti    (DInt a)    (DInt b) = DBool (a > b)
gti (DString a) (DString b) = DBool (a > b)

equ :: DValue S -> DValue S -> DValue S
equ    (DInt a)    (DInt b) = DBool (a == b)
equ   (DBool a)   (DBool b) = DBool (a == b)
equ (DString a) (DString b) = DBool (a == b)

overflow :: (() -> Integer) -> ([DValue S] -> t) -> t
overflow n c = 
    let i = n ()
    in c [DInt i]
