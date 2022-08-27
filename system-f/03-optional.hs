-- https://github.com/system-f/fp-course/blob/master/src/Course/Optional.hs

data Optional a =
  Full a
  | Empty
  deriving(Eq, Show)

{-
  Type constructor has "kind"

  ghci> :kind Optional
  Optional :: * -> *

  ghci> :kind Either
  Either :: * -> * -> *

  ghci> :kind Functor
  Functor :: (* -> *) -> Constraint
-}

{-
  A type hole: "_todo"

  fullOr :: a -> Optional a -> a
  fullOr a opt = 
    _todo

  Haskell compiler will find out what this hole should be:
  "Found hole: _todo :: a ... Valid hole fits include a :: a (bound at system-f/03-optional.hs:20:8)"
-}

-- | Return the possible value if it exists; otherwise, the first argument.
--
-- >>> fullOr 99 (Full 8)
-- 8
--
-- >>> fullOr 99 Empty
-- 99
fullOr :: a -> Optional a -> a
fullOr a Empty = a
fullOr _ (Full b) = b

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f Empty = Empty
mapOptional f (Full a) = Full (f a)

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional f Empty = Empty
bindOptional f (Full a) = f a

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :: Optional a -> Optional a -> Optional a
(<+>) Empty a = a 
(<+>) a _ = a 

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional :: (a -> b) -> b -> Optional a -> b
optional f b Empty = b
optional f b (Full a) = f a

-- | bindOptional with the transformer `f` as an Optinal as well
applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

-- | twiceOptional transform 2 Optionals to 1 Optional
-- 
-- it uses `.` operator to chain `applyOptional`, `mapOptional`
-- it is equqvalent to 
-- twiceOptional f optA optB = applyOptional (mapOptional f optA) optB
--
-- (mapOptional f optA) will return an Optional curry function as a transformer
twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

----------
----------
-- Haskell needs an entrypoint function, which returns an IO
-- https://stackoverflow.com/questions/50197631/haskell-parse-error-module-header-import-declaration-or-top-level-declaration
-- https://stackoverflow.com/questions/15482632/how-to-load-a-script-to-ghci

runTest = do
  print "(fullOr 99 (Full 8))"
  print (fullOr 99 (Full 8))
  -- 8
  print "(fullOr 99 Empty)"
  print (fullOr 99 Empty)
  -- 99
  print "(mapOptional (+1) (Full 1))"
  print (mapOptional (+1) (Full 1))
  -- Full 2
  print "(bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8))"
  print (bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8))
  -- Full 7
  print "(Full 2 <+> Empty)"
  print (Full 2 <+> Empty)
  -- Full 2
  print "(Full 10 <+> Full 3)"
  print (Full 10 <+> Full 3)
  -- Full 10
  print "(Empty <+> Full 2)"
  print (Empty <+> Full 2)
  -- Full 2