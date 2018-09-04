module RWH_Ch4Excercise1 where


import Data.List
import Data.Char (digitToInt)

asInt :: String -> Int
loop :: Int -> String -> Int

-- also works for hexa digits "ff" ->
asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x: xs) = let acc' = acc * 10 + digitToInt x
                   in loop acc' xs

-- rewrite using fold

asInt2 xs = foldr helper 0 xs
            where helper x acc | x == '-' = acc * (-1)
                               | otherwise = acc * 10 + (digitToInt x)


main = do
  print "hi"

