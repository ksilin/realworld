module RWH_Chapter3 where

import Data.Typeable

add a b = a + b


lastButOne a =
   if length a == 2
   then head a
   else lastButOne (tail a)



--   type ctor  value/data ctor
-- why are the type ctor and value ctor differently named?
-- only fo rillustrational purposes, in real code, both have the same name most of the times


data BookInfo = Book Int String [String] deriving (Show)

type BookId = Int
type Title = String
type Authors = [String]
-- record <syntax
data BookInfo2 = Book2 {bookId :: BookId , title:: Title , authors :: Authors} deriving (Show)

b = Book2 123 "sdefg" ["sdf", "dgg"]

bookTitle (Book2 _ title _) = title

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts -- eta reduce: pluralise word = map plural
  where plural :: Int -> String
        plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"

cheeses = pluralise "cheese" [0,1,2]

lend amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where reserve = 100
        newBalance = balance - amount

myLength :: Num p => [a] -> p
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

main = do
  putStrLn ("type of \"" ++ (show b) ++ "\" is: " ++ (show (typeOf b)))
  putStrLn ("type of \"" ++ (show (bookTitle b)) ++ "\" is: " ++ (show (typeOf (title b))))
  putStrLn (show cheeses)
