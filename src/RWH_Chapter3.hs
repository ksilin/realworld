-- module RWH_Chapter3 where

add a b = a + b


lastButOne a =
   if length a == 2
   then head a
   else lastButOne (tail a)

--   type ctor  value/data ctor
-- why are the type ctor and value ctor differently named?
-- only fo rillustrational purposes, in real code, both have the same name most of the times
data BookInfo = Book Int String [String] deriving (Show)
