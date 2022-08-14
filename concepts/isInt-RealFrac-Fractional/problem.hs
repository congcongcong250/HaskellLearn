module LeapYear (isDivisible) where

-- https://stackoverflow.com/questions/1164003/how-do-i-test-if-a-floating-point-number-is-an-integer-in-haskell
-- compiler type inference
-- isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

isDivisible a b = isInt (a / b)

isLeapYear :: Integer -> Bool
isLeapYear year = 
  if isDivisible year 4 then 
    if isDivisible year 400 then 
      True 
    else 
      if isDivisible year 100 then False else True 
  else 
    False

{-
/mnt/exercism-iteration/src/LeapYear.hs:8:6: error:
    • No instance for (RealFrac Integer) arising from a use of ‘isInt’
    • In the expression: isInt (year / 4)
      In the expression:
        if isInt (year / 4) then
            if isInt (year / 400) then
                True
            else
                if isInt (year / 100) then False else True
        else
            False
      In an equation for ‘isLeapYear’:
          isLeapYear year
            = if isInt (year / 4) then
                  if isInt (year / 400) then
                      True
                  else
                      if isInt (year / 100) then False else True
              else
                  False
  |
8 |   if isInt (year / 4) then 
  |      ^^^^^^^^^^^^^^^^

/mnt/exercism-iteration/src/LeapYear.hs:8:13: error:
    • No instance for (Fractional Integer) arising from a use of ‘/’
    • In the first argument of ‘isInt’, namely ‘(year / 4)’
      In the expression: isInt (year / 4)
      In the expression:
        if isInt (year / 4) then
            if isInt (year / 400) then
                True
            else
                if isInt (year / 100) then False else True
        else
            False
  |
8 |   if isInt (year / 4) then 
  |             ^^^^^^^^
-}