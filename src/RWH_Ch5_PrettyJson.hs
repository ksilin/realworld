module RWH_Ch5_PrettyJson where

import Data.List (intercalate)
import RWH_Ch5_Json
import Numeric


data Doc = ToBeDefined deriving (Show)
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <.> x <.> char right

(<.>) :: Doc -> Doc -> Doc
a <.> b = undefined

char :: Char -> Doc
char c = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double str = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar = undefined

renderJValue :: JValue -> Doc

renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)


