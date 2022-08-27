{-
  https://stackoverflow.com/questions/19497940/haskell-why-does-realfrac-not-imply-fractional
  ---------
  (/) :: (Fractional a) => a -> a -> a

  Integer does not fit in the following definition
  
  f :: Integer -> Bool
  f y = isInt (y / 4)

  Check out basic typeclass relationship in 
  https://www.haskell.org/onlinereport/basic.html
-}

-- To make it work, do a explicit casting for Integer
divInt intA intB = fromIntegral intA / fromIntegral intB

fFix1 :: Integer -> Bool
fFix1 y = isInt (divInt y 4)

fFix2 :: Integer -> Bool
fFix2 y = isInt (fromIntegral intA / fromIntegral intB)

-- General advice do not do math in this wa, avoid casting

