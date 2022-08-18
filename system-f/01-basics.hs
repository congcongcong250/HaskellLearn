-- Type infer
bool = True

-- Type annotation
number :: Int
number = 1

-- if must have an else
res = if True then 'a' else 'b'

-- Lambda
-- Return a function by a function
ifTrueElseSomething :: Bool -> Bool -> Bool
ifTrueElseSomething = \i -> \x -> if i then True else x

ifTrueElseSomething2 :: Bool -> (Bool -> Bool)
ifTrueElseSomething2 = \i -> (\x -> if i then True else x)

-- not :: Bool -> Bool
-- id :: a -> a
ifTrueElseFlip :: Bool -> (Bool -> Bool)
ifTrueElseFlip = \i -> if i then id else not
{-
  func = ifTrueElseFlip True
  res = func False

  res2 = ifTrueElseFlip True False
-}

-- Haskell compiler is smart, and it only allows single return type
ifTrueElseChar = \i -> \x -> if i then 'a' else x
-- !!!
-- x will be inferred as a Char
-- ifTrueElseChar :: Bool -> Char -> Char
-- !!!

-- Syntaxic Sugar for Function
ifTrueElseSomething3 :: Bool -> Bool -> Bool
ifTrueElseSomething3 = \i x -> if i then True else x

ifTrueElseSomething4 :: Bool -> Bool -> Bool
ifTrueElseSomething4 i x = if i then True else x

-- Prefix and infix function, with operator
three = 1 + 2
three2 = (+) 1 2

(.+.) :: Int -> Int -> String
(.+.) x y = 
  show (x + y)

myPlusRes = 2 .+. 2

fn x y = 
  x + y + 1

fnRes = 1 `fn` 2