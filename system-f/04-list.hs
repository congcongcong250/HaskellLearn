{-
  Define a CUSTOM list type
  - We want a `Cons`
  - Just Like `cons` in lisp: (cons 1 (cons 2 nil))
  - `Cons 2 Nil` will be a data
  - `Cons` is a "data constructor"
  
  data List t =
    Nil
    | Cons t (List t)
    deriving (Eq, Ord)

  -- We could use a operator (:.) to replace `Cons`

  data List t =
    Nil
    | (:.) t (List t)
    deriving (Eq, Ord)

  -- We could also infix it
-}
data List t =
  Nil
  | t :. List t
  deriving (Eq, Ord)

-- Now we can do
-- rawList1 = ((:.) 1 ((:.) 2 Nil))
-- rawList2 = (1 :. (2 :. Nil))

{-
  !!! To makes (1 :. 2 :. 3) possible
  !!! We need Right-associative with `infixr`
  It means (1 :. 2 :. 3) is equavlent to (1 :. (2 :. Nil))
  
  `infixr <precedence> <operator>`
  Ref: 
    - https://stackoverflow.com/questions/27770118/how-does-the-infix-work
    - https://devtut.github.io/haskell/fixity-declarations.html
-}

infixr 5 :.

-- Helper: foldRight, foldLeft, infinity

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  let inf x = x :. inf (x+1)
  in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)


foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- Transform our CUSTOM List in to a Haskell list 
-- by re-appending elements with `:`
hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

{-
  Implements an instance of Show type class, using hlist
  
  show = show . hlist
  show l = show (hlist l)

  Ref:
  - https://stackoverflow.com/questions/631284/dot-operator-in-haskell-need-more-explanation
-}
instance Show t => Show (List t) where
  show = show . hlist

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr :: a -> List a -> a
headOr a Nil = a
headOr _ (h :. t) = h

-- (++) :: List a -> List a -> List a
(++) :: List a -> List a-> List a
(++) = flip (foldRight (:.))

-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
flatten :: List (List a) -> List a
flatten = foldRight (+++) Nil

-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
-- 
-- flatMap f xs = flatten (map f xs)
flatMap :: (a -> List b) -> List a -> List b
flatMap f = flatten . map f

{-
  Why we don't need to do `flatten . (map f)`
  
  Due to the precedence of "space-operator" (infix function application) is 10
  and precedence of (.) is 9
  
  :info (.)
  infixr 9 .

  > The precedence of a prefix function is 10, 
    while that of an infix function is 9 by default. 
    This applies even to prefix functions written infix using backticks, for example `sqrt` has precedence 9. 
    Infix functions such as + written prefix (+) have precedence 10. 
    You can change the precedence of infix functions using a fixity declaration such as infixl + 6 
    (The highest precedence you can declare is 9)

  Ref:
  - https://stackoverflow.com/questions/69919781/haskell-function-composition-operator-and-space-operator-precedence
  - https://stackoverflow.com/questions/71031388/precedence-of-function-application#comment125567262_71031388
-}

flattenAgain :: List (List a) -> List a
flattenAgain = flatMap id

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1, 10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  error "todo: Course.List#seqOptional"


reverse = foldLeft (flip (:.)) Nil