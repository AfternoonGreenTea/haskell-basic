module Main (main) where

import qualified MyLib (someFunc)

myName :: String
myName = "Taro"
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("My name is" ++ myName)
  MyLib.someFunc
