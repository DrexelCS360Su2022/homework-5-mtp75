{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit x = (x - lastDigit x) `div` 10

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    |x > 0 && x < 10 = [x]
    |x >= 10 = toDigits(x `div` 10) ++ [x `mod` 10]



doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:z) = x:y*2:doubleEveryOther z


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:y) = sum(toDigits x) + sumDigits y

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther (reverseList(toDigits x))) `mod` 10 == 0

--
-- Problem 2
--

square x = x*x

pow :: (a -> a) -> Int -> a -> a
pow f 0 a = a
pow f x a = f(pow f (x - 1) a)


g :: Integer -> Integer
g 0 = 0
g n = n - (pow g 2 (n-1))


h :: Integer -> Integer
h 0 = 0


d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
