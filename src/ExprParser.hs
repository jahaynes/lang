module ExprParser where

import Combinators
import Core
import LexState
import Parser       (Parser (..), (<|>))
import Tokens       (Token (..))

import Data.Functor ((<&>))

-- Lowest precedence
parseExpr :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
parseExpr = (sumTerm >>= \(sp, a) -> compOp >>= \(_, op) -> parseExpr >>= \(_, b) -> pure (sp, EBinPrimOp op a b))
        <|> sumTerm
    where
    compOp = eqI <|> ltEqI <|> ltI <|> gtEqI <|> gtI

-- Next lowest precedence
sumTerm :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
sumTerm = (productTerm >>= \(sp, a) -> summishOp >>= \(_, op) -> sumTerm >>= \(_, b) -> pure (sp, EBinPrimOp op a b))
      <|> productTerm
    where
    summishOp = stitch <|> addI <|> subI --TODO will stitch be mistaken for +?

-- etc..
productTerm :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
productTerm = (apply >>= \(sp, a) -> mullishOp >>= \(_, op) -> productTerm >>= \(_, b) -> pure (sp, EBinPrimOp op a b))
          <|> apply
    where
    mullishOp = mulI

apply :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
apply = do
    (sp, e) <- nonApply
    es      <- map snd <$> many (nonApplyRighterThan sp)
    pure (sp, foldl EApp e es)

-- TODO how does this work and the simpler implementation doesn't work? backtracking? try?
-- TODO dedupe using the impl in Combinators
nonApplyRighterThan :: (Eq s, Show s) => SourcePos -> Parser [(SourcePos, Token s)] (SourcePos, Expr s)
nonApplyRighterThan (SourcePos _ c) = 
  Parser $ \s ->
    case runParser nonApply s of
      r@(Right (_, (SourcePos _ c', _))) ->
        if c' >= c
          then r
          else Left "too left"
      Left _ -> Left "faily"

nonApply :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
nonApply = eparen
       <|> err
       <|> eShow
       <|> ifThenElse
       <|> term
       <|> lambda
       <|> elet
       <|> (Parser $ \s -> Left $ "Could not turn into nonApply: " <> show s)

term :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
term = do
    (sp, t) <- literal <|> variable <|> dCons
    pure (sp, ETerm t)

variable :: Show s => Parser [(SourcePos, Token s)] (SourcePos, Term s)
variable = lower <&> \(sp, v) -> (sp, Var v)

schoenfinkel :: [s] -> Expr s -> Expr s
schoenfinkel vs body = foldr ELam body vs

token :: Eq s => Token s -> Parser [(SourcePos, Token s)] (SourcePos, Token s)
token x =
    Parser $ \s -> case s of
        []           -> Left "token: No more tokens"
        ((sp,t):ts') -> if t == x then Right (ts', (sp, t)) else Left "token mismatch"

lower :: Show s => Parser [(SourcePos, Token s)] (SourcePos, s)
lower =
    Parser $ \s -> case s of
        []                       -> Left "lower: No more tokens"
        ((sp, LowerIdent i):ts') -> Right (ts', (sp, i))
        other                    -> Left ("Not a lower: " ++ show other)

upper :: Show s => Parser [(SourcePos, Token s)] (SourcePos, s)
upper =
    Parser $ \s -> case s of
        []                       -> Left "upper: No more tokens"
        ((sp, UpperIdent i):ts') -> Right (ts', (sp, i))
        other                    -> Left ("Not a upper: " ++ show other)

literal :: Parser [(SourcePos, Token s)] (SourcePos, Term s)
literal =
    Parser $ \s -> case s of
        ((sp, TokInt i):ts')     -> Right (ts', (sp, LitInt i))
        ((sp, TokBool b):ts')    -> Right (ts', (sp, LitBool b))
        ((sp, TokString st):ts') -> Right (ts', (sp, LitString st))
        (_:_)                    -> Left "Not a literal (int)"
        []                       -> Left "Out of tokens"

dCons :: Show s => Parser [(SourcePos, Token s)] (SourcePos, Term s)
dCons = upper <&> \(sp, v) -> (sp, DCons v)

-- Currying happens here.
lambda :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
lambda = do
    (sp, _)   <- token Lambda
    params    <- map (unvar . snd) <$> many1 variable
    _         <- token Dot
    (_, body) <- parseExpr
    pure (sp, schoenfinkel params body)

-- Let/Letrec/Fix
elet :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
elet = do
    (sp,   _) <- token TLet
    negs      <- many1 variable
    _         <- token SingleEq
    (_, b)    <- parseExpr
    _         <- token In
    (_, c)    <- parseExpr
    let (a:vars') = map (unvar . snd) negs
        lam = schoenfinkel vars' b
    pure (sp, ELet a lam c)

unvar :: Show s => Term s -> s
unvar (Var s) = s
unvar       x = error $ "Not a var: " ++ show x

eparen :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
eparen = token LParen *> parseExpr <* token RParen

err :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
err = do
    (sp, _) <- token TErr
    ( _, e) <- nonApply
    pure (sp, EUnPrimOp Err e)

eShow :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
eShow = do
    (sp, _) <- token TShow
    ( _, e) <- nonApply
    pure (sp, EUnPrimOp EShow e)

ifThenElse :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
ifThenElse = do
    (sp, _) <- token If
    (_,  a) <- parseExpr
    _       <- token Then
    (_,  b) <- parseExpr
    _       <- token Else
    (_,  c) <- parseExpr
    pure (sp, IfThenElse a b c)

mulI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
mulI = token Times <&> \(sp, _) -> (sp, MulI)

addI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
addI = token Plus <&> \(sp, _) -> (sp, AddI)

subI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
subI = token Minus <&> \(sp, _) -> (sp, SubI)

stitch :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
stitch = token TStitch <&> \(sp, _) -> (sp, ConcatS)

eqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
eqI = token DoubleEq <&> \(sp, _) -> (sp, EqI)

ltEqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
ltEqI = token TLessEq <&> \(sp, _) -> (sp, LtEqI)

ltI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
ltI = token TLessThan <&> \(sp, _) -> (sp, LtI)

gtEqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
gtEqI = token TGreaterEq <&> \(sp, _) -> (sp, GtEqI)

gtI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
gtI = token TGreaterThan <&> \(sp, _) -> (sp, GtI)

