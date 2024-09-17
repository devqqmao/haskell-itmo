-- Church-encoded booleans
tru = \x y -> x
fls = \x y -> y

-- Convert Church boolean to Haskell boolean
churchBoolToHaskell :: (a -> b -> a) -> Bool
churchBoolToHaskell b = b True False

-- Church-encoded numerals
toChurch :: Int -> (a -> a) -> a -> a
toChurch 0 = \f x -> x
toChurch n = \f x -> f (toChurch (n - 1) f x)

fromChurch :: (a -> a) -> a -> Int
fromChurch n = n (\x -> x + 1) 0

-- Successor function for Church numerals
succ = \n f x -> f (n f x)

-- Predecessor function (for decrementing by 1)
churchPred = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)

-- Subtract 3 by applying churchPred three times
subtract3 = \n -> churchPred (churchPred (churchPred n))

-- Check if the Church numeral is zero (used for divisibility check)
isZero = \n -> n (\x -> fls) tru

-- Divides3 function
divides3 = \n -> churchBoolToHaskell (isZero (subtract3 n))

-- Test cases
test = do
  print (divides3 (toChurch 0))  -- Should be True (1)
  print (divides3 (toChurch 1))  -- Should be False (0)
  print (divides3 (toChurch 2))  -- Should be False (0)
  print (divides3 (toChurch 3))  -- Should be True (1)
  print (divides3 (toChurch 4))  -- Should be False (0)
  print (divides3 (toChurch 5))  -- Should be False (0)
  print (divides3 (toChurch 6))  -- Should be True (1)

main = test

