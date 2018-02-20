module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Data/Aeson/IP.hs"]
