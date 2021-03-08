-- Orhun Dogan
-- November 22nd 2020
-- Haskell Lab Sheet 1

import Data.Char


square :: Int -> Int    -- Function that finds squares of numbers. Call by sqaure x ==> square 9 
square x = x * x

pyth :: Int -> Int -> Int   -- Function that adds squares of two inputs. Call by pyth x y ==> pyth 3 4 = 25
pyth x y = (square x) + (square y)

isTriple :: Int -> Int -> Int -> Bool   -- Function that chekcs if inputted side lengths make a right triangle ==> isTriple  3 4 5 
isTriple x y z = (square z) == (pyth x y)   -- Hypothenuse should be entered last

isTripleAny :: Int -> Int -> Int -> Bool    -- Same function as isTriple. The order of inputs does not matter
isTripleAny x y z = if x > y && x > z
    then (isTriple y z x)
    else if y > x && y > z
        then (isTriple  x z y)
        else (isTriple x y z)

halfEvens :: [Int] -> [Int]     -- Prints a list. If the element is even, it divides that element by 2.
halfEvens xs = [if even x then div x 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]     -- Return all numbers in the input list within the range given by two integers
inRange y z [] = []
inRange y z xs = [x | x <- xs, x >= y && x <= z]

countPositives :: [Int] -> Int      -- Calculates the amount of positive integers in a list
countPositives [] = 0
countPositives xs = sum[1 | x <- xs, x>0]

capitalised :: String -> String     -- Makes the first letter uppercase, rest lowercase
capitalised xs = [toUpper y | y <- xs, y == head xs] ++ [toLower x | x <-xs, x /= head xs]

lowercased :: String -> String          -- Makes every letter lowercase
lowercased word = [toLower w | w <- word]

title :: [String] -> [String]
title [] = []
title xs = [if length x > 3 || x == head xs
    then capitalised x
    else lowercased x|x<-xs]
