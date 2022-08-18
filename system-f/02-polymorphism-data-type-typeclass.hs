-- Prelude is imported implicitly for every file
-- it contains Show, Int, String, etc
-- import Prelude

-- Data type
{-
 ' is a valid character in name
 Sth' in convention means it's a new version of Sth
-}
data Bool'
  = True'
  | False'
  deriving Show
{-
  Bool' is a "data type", it is a "type"
  True' and False' are "data constructor", it create a piece of "data"/"value"
  ---
  ghci> :t True'
  True' :: Bool'
  ---
  Ref: https://stackoverflow.com/questions/18204308/haskell-type-vs-data-constructor
-}
{-
  `deriving Show` is the magic to tell ghci how to show the type
  otherwise compiler will give "No instance for (Show Bool') arising from a use of ‘print’"
-}

-- Data constructor with arguments
data Attribute
  = Height Int
  | Name String
  | Meta String
  deriving Show
{-
  ghci> :t Height 175
  Height 175 :: Attribute

  ghci> :t Name "Lee"
  Name "Lee" :: Attribute
  
  leeAttribute :: Attribute
  leeAttribute = Height 175
-}

-- Usage: pattern matching and deconstruction
f :: Attribute -> Int
f = 
  \attr -> 
    case attr of
      Height h -> h
      Name n -> length n
      Meta _ -> 0 -- _ means we don't care
{-
  ghci> f (Height 175) 
  175

  ghci> f (Name "Lee")
  3
-}

-- Inline pattern matching
inlineF :: Attribute -> Int
inlineF (Height h) = h
inlineF (Name n) = length n
inlineF (Meta _) = 0

-- Two namespaces
{-
  1. Type namespace
  2. Value namespace
  ---
  Same name in 2 difference namespaces will not clash
  Same name in the same namespace will clash
-}

-- B in type level
-- Usage `variable :: B`
data B = BB Int; 

-- B in value level
-- Usage `variable = B 12`
data X = B Int

{- All types must start with Uppercase unless it's polymorphic -}

-- Type constructor for polymorphism
data Thing a
  = X a
  | Y Char Char 
  deriving Show

{-
  `Thing` is a "type constructor", similar to `Thing<A>`
  `Thing Int` is a "type", similar to `Thing<Int>`

  ghci> :type Y '1' '2'
  Y '1' '2' :: Thing a

  ghci> :type X 12
  X 12 :: Thing Int
-}
f2 :: Thing Int -> Int
f2 (X i) = i
f2 (Y _ _) = 0

f3 :: Thing Char -> Char
f3 (X c) = c
f3 (Y _ c) = c

f4 :: Thing a -> Bool
f4 (X _) = True
f4 (Y _ _) = False

{-
  f5 :: Thing a -> Bool
  f5 (X v) = 
    if v then 1 else 2
  f5 (Y _ _) = 
    0
  
  !!! It will not compile because 
  `if a then 1 else 2` expected v :: Bool
  while the definition defines a polymorphic v :: a

  "Couldn't match expected type ‘Bool’ with actual type ‘a’"
-}

-- Type classes
{-
  f6 :: Thing a -> Int
  f6 (X v) = 
    length (show v)
  f6 (Y _ _) = 
    0

  !!! It will not compile because
  No instance for (Show a) arising from a use of ‘show’
-}

-- Type class constraint
-- a is any type, as long as it's an instance of Show type class
lengthFromX :: Show a => Thing a -> Int
lengthFromX (X v) = 
  length (show v)
lengthFromX (Y _ _) = 
  0

multipleConstraints :: (Show a, Eq a, Ord a) => Thing a -> Int
multipleConstraints (X v) = 
  length (show v)
multipleConstraints (Y _ _) = 
  0

-- Make type class
class ToString a where
  toString :: a -> String

instance ToString Bool where
  toString True = "True"
  toString False = "False"

instance ToString Int where
  toString = show

b :: ToString a => a -> String
b x = toString x
{-
   ghci> b (1 :: Int)
   "1"

   ghci> b True
   "True"
-}

