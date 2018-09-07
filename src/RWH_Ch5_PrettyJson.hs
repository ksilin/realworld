module RWH_Ch5_PrettyJson where

import Data.List (intercalate)
import RWH_Ch5_Json
import Numeric
import Data.Bits
import Data.Char


data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

-- string is written in a point-free style, by combining fns
-- 'point' is roughly synonymous with 'value' -> a point-free expression makes no mention of values it operates on
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <.> x <.> char right

(<.>) :: Doc -> Doc -> Doc
Empty <.> y = y
x <.> Empty = x
x <.> y = x `Concat` y


char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text str = Text str

double :: Double -> Doc
double d = text (show d)

hcat :: [Doc] -> Doc
hcat xs = undefined

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

line :: Doc
line = Line

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise -> char c
            where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                where ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <.> text (replicate (4 - length h) '0')
           <.> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <.> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep xs = undefined


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <.> p) : punctuate p ds

--renderJValue :: JValue -> Doc
--
--renderJValue (JString s) = string s
--renderJValue (JNumber n) = double n
--renderJValue (JBool True) = text "true"
--renderJValue (JBool False) = text "false"
--renderJValue JNull = text "null"
--
--renderJValue (JObject o) = "{" ++ pairs o ++ "}"
--  where pairs [] = ""
--        pairs ps = intercalate ", " (map renderPair ps)
--        renderPair (k, v) = show k ++ ": " ++ renderJValue v
--
--renderJValue (JArray a) = "[" ++ values a ++ "]"
--  where values [] = ""
--        values vs = intercalate ", " (map renderJValue vs)


