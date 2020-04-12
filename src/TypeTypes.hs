{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeTypes where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Map (Map)

newtype TVar s = TV s
  deriving (Eq, Ord)

data TCon s = TC s [TVar s]
  deriving (Eq, Ord, Show)

instance Show s => Show (TVar s) where
  show (TV s) = show s

data Env s = TypeEnv { types :: Map s (Scheme s) }
  deriving Eq

newtype Subst s = Subst (Map (TVar s) (Type s))
  deriving (Eq, Ord, Semigroup, Monoid)

data Type s
  = TVar (TVar s)
  | TCon (TCon s)
  | TArr (Type s) (Type s)
  deriving (Eq, Ord)

instance Show s => Show (Type s) where
  show (TVar (TV s)) = show s
  show (TCon s)      = show s
  show (TArr a b)    = unwords [show a, "->", show b]

data Scheme s = Forall [TVar s] (Type s)
  deriving (Eq, Ord)

instance Show s => Show (Scheme s) where
  show (Forall [] typ) = show typ
  show (Forall vs typ) = "forall " ++ (unwords $ map show vs) ++ ". " ++ show typ 

class FromString a where
    fromString :: String -> a

typeInt, typeBool, typeString :: FromString s => Type s
typeInt    = TCon $ TC (fromString "Int"   ) []
typeBool   = TCon $ TC (fromString "Bool"  ) []
typeString = TCon $ TC (fromString "String") []

--TODO [List a] = TArr List (TVar a) -- or something? need to parse variables in data definition
typeObject objName = TCon objName


instance FromString ByteString where
  fromString = C8.pack