module RWH_Ch4 where

import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))



-- why is tail recursion important? Bc in imperative langs, loops execute inconstant space,
-- while non-tail recursion executes in linear space.
-- tail recursion can be optimized by the compiler to run in constant space (TCO).
-- If your language does not perform TCO, use recursion at your own risk



-- filter out all even numbers
oddList :: [Int] -> [Int]

oddList (x:xs) | odd x = x : oddList xs
               | otherwise = oddList xs
oddList _ = []

--

sumList xs = helper 0 xs -- eta reduce: sumList = helper 0
  where helper acc (x:xs) = helper (acc + x) xs
        helper acc _ = acc

--

base = 65521
adler32 xs = helper 1 0 xs
  where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                b' = (a' + b) `mod` base
                            in helper a' b' xs
        helper a b _ = (b `shiftL` 16) .|. a

-- using both accumulators in a tuple
-- it makes almost no difference except we now can use the fold fn
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldl step zero (x:xs) = foldl step (step zero x) xs
-- foldl _ zero = zero
-- step takes an acc and an element of list and retruns an updated acc
adler321 xs = helper (1,0) xs
  where helper (a, b) (x:xs) =
          let a' = (a + (ord x .&. 0xff)) `mod` base -- ord makes fn applicable to Strings only
              b' = (a' + b) `mod` base
          in helper (a', b') xs
        helper (a, b) _ = (b `shiftL` 16) .|. a

-- rewrite of sumList using foldl
sumList2 xs = foldl step 0 xs
  where step acc x = acc + x

-- we can also reference the addition op directly
sumList3 xs = foldl (+) 0 xs

-- or sumList3 = foldl (+) 0 after eta reduction, but this variant does not compile

adler322 xs = let (a, b) = foldl step (1, 0) xs
              in  (b `shiftL` 16) .|. a
              where step (a, b) x = let a' = a + (ord x .&. 0xff)
                                    in (a' `mod`  base, (a' + b) `mod` base)


-- inpractice, we never use foldl, only foldr
-- thunking with foldl is expensive, in time and space
-- foldl (+) 0 [1..1000000] -> stack overflow = space leak

-- Data.List defines foldl' that does not thunk
-- foldl' OTOH is not lazy - take care on infinite lists

-- using filter from prelude:
main = do
  print "---"
  print (oddList [1, 2, 3, 4, 5])
  print "---"
  print (filter odd [1, 2, 3, 4, 5])


