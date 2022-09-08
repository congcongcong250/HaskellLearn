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

-- Helper: foldRight, foldLeft
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