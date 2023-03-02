module Main where

import           System.Environment       (getArgs)

import qualified Utils                    (unsafeReadAddress, unsafeGetPaymentPubKeyHash)

--Modulo: 

main :: IO ()
main = do
    [addr'] <- getArgs
    print $ Utils.unsafeGetPaymentPubKeyHash $ Utils.unsafeReadAddress addr'
