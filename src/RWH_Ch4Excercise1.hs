module RWH_Ch4Excercise1 where


import Data.List
import Data.Char

asInt :: String -> Int
loop :: Int -> String -> Int

-- also works for hexa digits "ff" ->
asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x: xs) = let acc' = acc * 10 + digitToInt x
                   in loop acc' xs

-- rewrite using fold

asInt2 ('-':xs) = -1 * asInt2 xs
asInt2 xs = fst (foldr helper (0, 1) xs)
  where
    helper x (acc, factor) = (acc + digitToInt x * factor, factor * 10)

fn = \x (acc, factor) -> (acc + digitToInt x * factor, factor * 10)
asInt2Lambda xs = fst (foldr fn (0, 1) xs)

--asIntEither ('-':xs) = -1 * asIntEither xs
--asIntEither xs = fst (foldr helper Right(0, 1) xs)
--  where
--    helper x (acc, factor) | isDigit x = Right(acc + digitToInt x * factor, factor * 10)
--                           | otherwise = Left (show x ++ "is not a digit")

simpleEither x | isDigit x = Right(digitToInt x + 2)
               | otherwise = Left(show x ++ " is not a number")

-- 3 ex
-- ananymous fns only support a single pattern, be sure to make it exhaustive and unfailable
concat2 xs = foldr (\acc x -> acc ++ x) [] xs

-- compose and the . shortcut
capitalizedCount = length . filter (isUpper . head). words

main = do
  print "hi"

-- tail recursion and anon fns are nice, but we dont use them too often, we prefer lib fns like 'map' & 'fold'
-- one of the reasons is power - a tail-rec fn is powerful - it can dy anthing it likes
-- while the sematics of map are more constricted, while folds lie inbetween
-- anon fns interrupt code reading flow, local fn def is easy, no reason not to document your lambdas

-- seq forces first arg to be evaluated
-- seq must be the first expression to be evaluated, otherwise it will have no effect
-- seq stops as soon it reaches a ctor, e.g. (1 + 2) : (2 + 3) : [] will only eval the first parens
-- more in Ch25

