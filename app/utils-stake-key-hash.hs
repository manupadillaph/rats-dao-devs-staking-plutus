module Main where

import System.Environment (getArgs)
import qualified Utils (unsafeReadAddress, unsafeGetStakePubKeyHash)

--Modulo:

main :: IO ()
main = do
  [addr'] <- getArgs
  print $ Utils.unsafeGetStakePubKeyHash $ Utils.unsafeReadAddress addr'
