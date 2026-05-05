module Main (main) where

import Gilear.Spec (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  someFunc
