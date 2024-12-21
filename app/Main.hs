module Main (main) where

import Linear
import Lib

main :: IO ()
main = do
  let a :: V2 Double = V2 0.2 0
      b = V2 0.5 1
      ps = march a b
  print $ take 5 ps
