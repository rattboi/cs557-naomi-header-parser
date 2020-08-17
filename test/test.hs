module Main where

import NaomiHeaderParser
import qualified Data.ByteString as B

main :: IO ()
main = do
  borderdown <- B.readFile "./bd.header"
  printNaomiHeader (parseNaomiHeader borderdown)
  crazytaxi <- B.readFile "./ct.header"
  printNaomiHeader (parseNaomiHeader crazytaxi)
  mvc2 <- B.readFile "./mvc2.header"
  printNaomiHeader (parseNaomiHeader mvc2)
  monkeyball <- B.readFile "./mb.header"
  printNaomiHeader (parseNaomiHeader monkeyball)
  -- Last one is the fake header I built up for testing, before figuring out reading from files
  printNaomiHeader (parseNaomiHeader fakeHeader)
