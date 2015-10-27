module TestMain where

import Main hiding (main)
import Test.Framework

main :: IO ()
main = defaultMain []
