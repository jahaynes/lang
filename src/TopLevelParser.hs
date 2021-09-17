module TopLevelParser where

import Combinators
import Core
import ExprParser
import LexState
import Parser
import Tokens      (Token (..))
import TypeTypes

parseTopLevel :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, TopLevel s)
parseTopLevel = do
    ((sp, d):ds) <- many1 (parseDataDef <|> parseFunDef)
    pure (sp, TopLevel (d:map snd ds))

parseDataDef :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Defn s)
parseDataDef = do

    {- data -}
    _           <- token TData
    (sp, tyCon) <- parseTypeConstructor

    {- = -}
    _          <- token SingleEq

    {- data constructors -}
    (_, first) <- parseDataConstructor
    rest <- map snd <$> many (token TPipe *> parseDataConstructor)

    pure (sp, DataDef tyCon (first:rest))

{- 
    List a
    TC "List" ["a"]
-}
parseTypeConstructor :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, TCon s)    
parseTypeConstructor = do
    (sp@(SourcePos _ col), u) <- upper
    vs      <- map snd <$> manyIncreasinglyRight (Just col) tyVar
    pure (sp, TC u vs)

{- 
    Nil
    DCons "Nil" []

    Cons a (List a)
    DCons "Cons" ["a", TC "List" ["a"]]
-}
parseDataConstructor :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, DataCons s)
parseDataConstructor = do
    (sp@(SourcePos _ col), u) <- upper
    vs      <- map snd <$> manyIncreasinglyRight (Just col)
                             ( ( aa <$> tyVar)
                           <|> ( bb <$> parseParenTypeConstructor)
                             )
    pure (sp, DataCons u vs)
    where
    aa :: (SourcePos, TVar s) -> (SourcePos, Type s)
    aa (sp, tv) = (sp, TVar tv)
    bb :: (SourcePos, TCon s) -> (SourcePos, Type s)
    bb (sp, tc) = (sp, TCon tc)

parseParenTypeConstructor :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, TCon s)    
parseParenTypeConstructor =
    paren parseParenTypeConstructor <|> parseTypeConstructor


paren :: (Eq s, Show s)
      => Parser [(SourcePos, Token s)] (SourcePos, a)
      -> Parser [(SourcePos, Token s)] (SourcePos, a)
paren p = token LParen *> p <* token RParen

tyVar :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, TVar s)
tyVar = (\(sp, x) -> (sp, TV x)) <$> lower

parseFunDef :: (Eq s, Show s) => Parser [(SourcePos, Token s)] (SourcePos, Defn s)
parseFunDef = do
    (sp, name):negs <- many1 lower
    let vars = map snd negs
    _         <- token SingleEq
    (_, expr) <- parseExpr
    let lam = ELam vars expr
    pure (sp, FunDef name lam)
