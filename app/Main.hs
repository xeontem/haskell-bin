module Main where

import Lib
import Data.Char
import Data.Function
test = isDigit '7'

main :: IO ()
main = someFunc
{- 
  literals for custom operator: !#$%&*+./<>=?@\^|-~
-}
infixl 6 *+* -- my custom operator sum of squares
a *+* b = a ^ 2 + b ^ 2 -- define it body. operator just like a function of two args
-- ----------------------------------------------------------

f % x = f x -- define operator as apply func to her arg. the operator has a zero priority.

identity x = x 

four = prod 2 3 

-- example
{- f(g x (h y)) == f $ g x (h y) == f $ g x $ h y -}
sum x y = x + y
prod x y = x * y
-- prod sum 2 3 sum 4 5

-- ------------------------- roots square equation ------------------------------------------------------------- 
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = 
  (
    (- b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a),
    (- b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )

roots' a b c = let
  d = sqrt (b ^ 2 - 4 * a * c)
  -- putStrLn d
  in ((- b - d) / (2 * a), (- b + d) / (2 * a))

roots'' a b c = 
  let {
    d = sqrt (b ^ 2 - 4 * a * c);
    x1 = (- b - d) / (2 * a);
    x2 = (- b + d) / (2 * a);
  } in (x1, x2)

roots''' a b c = 
  let
    d = sqrt (b ^ 2 - 4 * a * c)
    x1 = (- b - d) / aTwice
    x2 = (- b + d) / aTwice
    aTwice = 2 * a
  in (x1, x2)
-- return differense between roots ----------------------
rootsDiff a b c = let
  (x1, x2) = roots a b c
  in x2 - x1

diffs a b = b - a

diff2 = let
  (x1, x2) = roots 1 9 2
  in diffs x1 x2

diff3 = diffs x1 x2 where
  (x1, x2) = roots 1 9 2

diff = rootsDiff 3 4 5

guard n
  | n == 1 = "guard 1"
  | n == 2 = "guard 2"
  | otherwise = "guard" ++ "otherwise"

-- factorial -------------------------------
factorial6 n
  | n >= 0 = let
      helper acc 0 = acc
      helper acc n = helper (acc * n) (n - 1)
    in helper 1 n
  | otherwise = error "arg must be >= 0"


right a = [a]

-- ---------------------------------- polimorph parametric
id:: a -> a
id x = x

-- apply2:: a -> b -> a -> b
apply2 f x = f % f x

-- flip f x y = f y x

true = flip const 5 True

six = apply2 (+ 2) 2

abcdcd = apply2 (++ "CD") "AB"

---------- anonimus (lambda) functions ---------------------------------------------
-- (\x -> 2 * x + 7) 10 -- lambda function

f = \x -> 2 * x + 7

lenVec x y = sqrt (x^2 + y^2)
lenVec' x = \y -> sqrt (x^2 + y^2)
lenVec'' = \x -> \y -> sqrt (x^2 + y^2)
lenVec''' = \x y -> sqrt (x^2 + y^2) 

------------------- Higher order funcs --------------------------

p1 = ((1,2), (3,4))
p2 = ((3,4), (5,6))

sumFstFst = (+) `on` helper
  where helper pp = fst (fst pp)

sumFstFst' p1 p2 = fst % fst p1 + fst % fst p2
sumFstFst'' = \p1 p2 -> fst%fst p1 + fst%fst p2


summ = sumFstFst' p1 p2

first = fst (fst p1)
-------------------------------------------------------------------

compose f g x = f%g x

sumFstFst''' = \p1 p2 -> compose fst fst p1 + compose fst fst p2

workFst = sumFstFst''' p1 p2

