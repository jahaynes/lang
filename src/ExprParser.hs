module ExprParser where

import Combinators
import Core
import LexState
import Parser      (Parser (..), (<|>))
import Tokens      (Token (..))

import Data.Monoid ((<>))

parseExpr :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
parseExpr = apply <|> nonApply

  where
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
nonApply = paren
       <|> err
       <|> eShow
       <|> ifThenElse
       <|> lambda
       <|> elet
       <|> term
       <|> eBinPrimOp
       <|> (Parser $ \s -> Left $ "Could not turn into nonApply: " <> show s)

  where
  paren :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
  paren = token LParen *> parseExpr <* token RParen

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
  unvar x = error $ "Not a var: " ++ show x

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

  eBinPrimOp :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
  eBinPrimOp = do
    (sp, o) <- op
    (_, e1) <- nonApply
    (_, e2) <- nonApply
    pure (sp, EBinPrimOp o e1 e2)

  op :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
  op = addI <|> subI <|> mulI <|> eqI <|> stitch <|> ltEqI <|> ltI <|> gtEqI <|> gtI
    where
    addI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    addI = do
      (sp, _) <- token Plus
      pure (sp, AddI)

    subI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    subI = do
      (sp, _) <- token Minus
      pure (sp, SubI)

    mulI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    mulI = do
      (sp, _) <- token Times
      pure (sp, MulI)

    eqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    eqI = do
      (sp, _) <- token DoubleEq
      pure (sp, EqI)

    stitch :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    stitch = do
      (sp, _) <- token TStitch
      pure (sp, ConcatS)

    ltEqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    ltEqI = do
      (sp, _) <- token TLessEq
      pure (sp, LtEqI)

    ltI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    ltI = do
      (sp, _) <- token TLessThan
      pure (sp, LtI)

    gtEqI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    gtEqI = do
      (sp, _) <- token TGreaterEq
      pure (sp, GtEqI)

    gtI :: Eq s => Parser [(SourcePos, Token s)] (SourcePos, BinOp)
    gtI = do
      (sp, _) <- token TGreaterThan
      pure (sp, GtI)



  term :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Expr s)
  term = do
    (sp, t) <- literal <|> variable <|> dCons
    pure (sp, ETerm t)

  literal :: Parser [(SourcePos, Token s)] (SourcePos, Term s)
  literal = Parser $ \s -> case s of
    ((sp, TokInt i):ts')    -> Right (ts', (sp, LitInt i))
    ((sp, TokBool b):ts')   -> Right (ts', (sp, LitBool b))
    ((sp, TokString st):ts') -> Right (ts', (sp, LitString st))
    (_:_)                   -> Left "Not a literal (int)"
    []                      -> Left "Out of tokens"

  variable :: Show s => Parser [(SourcePos, Token s)] (SourcePos, Term s)
  variable = do
    (sp, v) <- lower
    pure (sp, Var v)

  dCons :: Show s => Parser [(SourcePos, Token s)] (SourcePos, Term s)
  dCons = do
    (sp, v) <- upper
    pure (sp, DCons v)

lower :: Show s => Parser [(SourcePos, Token s)] (SourcePos, s)
lower = Parser $ \s -> case s of
  []                       -> Left "lower: No more tokens"
  ((sp, LowerIdent i):ts') -> Right (ts', (sp, i))
  other                    -> Left ("Not a lower: " ++ show other)

upper :: Show s => Parser [(SourcePos, Token s)] (SourcePos, s)
upper = Parser $ \s -> case s of
  []                       -> Left "upper: No more tokens"
  ((sp, UpperIdent i):ts') -> Right (ts', (sp, i))
  other                    -> Left ("Not a upper: " ++ show other)

token :: Eq s => Token s -> Parser [(SourcePos, Token s)] (SourcePos, Token s)
token x = Parser $ \s -> case s of
  []           -> Left "token: No more tokens"
  ((sp,t):ts') -> if t == x then Right (ts', (sp, t)) else Left "token mismatch"

schoenfinkel :: [s] -> Expr s -> Expr s
schoenfinkel vs body = foldr ELam body vs