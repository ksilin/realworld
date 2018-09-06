module RWH_Ch5_JsonMain (main) where

-- since ghc expects the main to be named Main, you have to explicitly give it the name of your main module
-- stack ghc -- -o jsonSimple RWH_Ch5_JsonMain.hs -main-is RWH_Ch5_JsonMain
-- contrary to the instructions in RWH I did not have to list the used module files explicitly

import RWH_Ch5_Json
import RWH_Ch5_PutJson

obj = JObject [("foo", JNumber 1), ("bar", JBool False)]

main = do
  putStrLn (show obj)
  putStrLn (renderJValue obj)




