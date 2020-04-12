module Pretty where

class Pretty a where
    pretty :: Int -> a -> (IsCompound, String)

data IsCompound = IsSimple
                | IsCompound

simple :: a -> (IsCompound, a)
simple x = (IsSimple, x)

compound :: [String] -> (IsCompound, String)
compound xs = (IsCompound, unwords xs)

paren :: (IsCompound, String) -> String
paren (IsSimple,   x) = x
paren (IsCompound, x) = mconcat ["(", x, ")"]
