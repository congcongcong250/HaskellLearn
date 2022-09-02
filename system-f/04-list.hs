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