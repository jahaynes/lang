{-# LANGUAGE ScopedTypeVariables #-}

module CpsEval where

import CpsTypes

import Data.Word (Word64)
import qualified Data.ByteString.Char8 as C8

data Loc = Loc Word64 deriving Eq

data Store s = Store Loc (Loc -> DValue s) (Loc -> Int)

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
vee :: Env s -> Val s -> DValue s
vee   _   (VInt i)    = DInt i
vee   _   (VBool b)   = DBool b
vee   _   (VString s) = DString s
vee env v@(VVar _)    = env v

bind :: Eq s => Env s -> Val s -> DValue s -> Env s
bind env v@(VVar _) d = \w ->
    if v == w then d else env w

bindn :: Eq s => Env s -> [Val s] -> [DValue s] -> Env s
bindn env     []     [] = env
bindn env (v:vs) (d:ds) = bindn (bind env v d) vs ds

env0 :: Show s => Env s
env0 = \k -> error $ unwords [ "Could not find"
                             , show k
                             , "in env"
                             ]
{-
-- Yields answer by evaluating cexp in env where vars are bound to vals
cpsEval :: (Eq s, Semigroup s, Show s)
        => [Val s]      -- list of CPS vars
        -> Cexp s       -- a continuation expr
        -> [DValue s]   -- list of denotable vals
        -> Store s      -- a store
        -> Answer s
-}
cpsEval vs e ds = ee e (bindn env0 vs ds)

{-
--p34. Ee takes the denotation of a CPS expr
ee :: (Eq s, Semigroup s, Show s)
   => Cexp s
   -> Env s
   -> Store s
   -> Answer s
-}
ee (CApp f vl) env =
    let Func g = vee env f
    in g (map (vee env) vl)

ee (CFix fl e) env = ee e (g env)

    where
    g r = do
        let funNames = map (\(n,_,_) -> n) fl
            funVals  = map (h r) fl
        bindn r funNames funVals

    h rl (_, vl, b) = Func $ \al -> ee b (bindn (g rl) vl al)

ee (CPrimOp p vl wl el) env =
    evalprim p
             (map (vee env) vl)
             (map (\e -> \al -> ee e (bindn env wl al)) el)

ee (CSwitch v t f) env =
    case vee env v of
        DBool True  -> ee t env
        DBool False -> ee f env
        other -> error "Unexpected switch on nonbool"

ee (CHalt val) env = \s ->
    case vee env val of
        a@(DInt _)    -> Answer a
        a@(DBool _)   -> Answer a
        a@(DString _) -> Answer a
        Func _ ->
            error " I dont think this is part of the semantics "

--evalprim :: (Eq s, Show s, Semigroup s) => COp -> [DValue s] -> [[DValue s] -> Store s -> Answer s] -> Store s -> Answer s

evalprim    AddI       [DInt i, DInt j] [c] = overflow (\() -> i + j) c
evalprim    SubI       [DInt i, DInt j] [c] = overflow (\() -> i - j) c
evalprim    MulI       [DInt i, DInt j] [c] = overflow (\() -> i * j) c

evalprim     LtEqI     [     a,      b] [c] = c [lteqi a b]
evalprim     LtI       [     a,      b] [c] = c [lti   a b]
evalprim     GtEqI     [     a,      b] [c] = c [gteqi a b]
evalprim     GtI       [     a,      b] [c] = c [gti   a b]

evalprim     EqI       [     a,      b] [c] = c [equ   a b]

evalprim ConcatS [DString a, DString b] [c] = c [DString $ a <> b]



evalprim    EShow    [DInt i]  [c] = c [DString . C8.pack . show $ i]
evalprim    EShow   [DBool b]  [c] = c [DString . C8.pack . show $ b]
evalprim    EShow [DString s]  [c] = c [DString . C8.pack . show $ s]
evalprim    Err [DString e] [c] = error $ "ERROR: " ++ show e

evalprim a b _ = error $ show (a,b)

lteqi :: (Eq s, Ord s, Show s) => DValue s -> DValue s -> DValue s
lteqi    (DInt a)    (DInt b) = DBool (a <= b)
lteqi (DString a) (DString b) = DBool (a <= b)

lti :: (Eq s, Ord s, Show s) => DValue s -> DValue s -> DValue s
lti    (DInt a)    (DInt b) = DBool (a < b)
lti (DString a) (DString b) = DBool (a < b)

gteqi :: (Eq s, Ord s, Show s) => DValue s -> DValue s -> DValue s
gteqi    (DInt a)    (DInt b) = DBool (a >= b)
gteqi (DString a) (DString b) = DBool (a >= b)

gti :: (Eq s, Ord s, Show s) => DValue s -> DValue s -> DValue s
gti    (DInt a)    (DInt b) = DBool (a > b)
gti (DString a) (DString b) = DBool (a > b)

equ :: (Eq s, Show s) => DValue s -> DValue s -> DValue s
equ    (DInt a)    (DInt b) = DBool (a == b)
equ   (DBool a)   (DBool b) = DBool (a == b)
equ (DString a) (DString b) = DBool (a == b)

overflow :: (() -> Integer) -> ([DValue s] -> t) -> t
overflow n c = 
    let i = n ()
    in c [DInt i]