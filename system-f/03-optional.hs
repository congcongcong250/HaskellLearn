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

fullOr :: a -> Optional a -> a
fullOr a Empty = a
fullOr _ (Full b) = b

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f Empty = Empty
mapOptional f (Full a) = Full (f a)
-- mapOptional (+1) (Full 1)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional f Empty = Empty
bindOptional f (Full a) = f a
-- bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)

(<+>) :: Optional a -> Optional a -> Optional a
(<+>) Empty a = a 
(<+>) a _ = a 
-- Full 2 <+> Empty
-- Full 10 <+> Full 3
-- Empty <+> Full 2

-- Haskell needs an entrypoint function, which returns an IO
-- https://stackoverflow.com/questions/50197631/haskell-parse-error-module-header-import-declaration-or-top-level-declaration
-- https://stackoverflow.com/questions/15482632/how-to-load-a-script-to-ghci

runTest = do
  print (fullOr 99 (Full 8))
  -- 8
  print (fullOr 99 Empty)
  -- 99
  print (mapOptional (+1) (Full 1))
  -- Full 2
  print (bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8))
  -- Full 7
  print (Full 2 <+> Empty)
  -- Full 2
  print (Full 10 <+> Full 3)
  -- Full 10
  print (Empty <+> Full 2)
  -- Full 2

-- TODO