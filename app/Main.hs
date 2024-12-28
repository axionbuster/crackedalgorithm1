module Main (main) where

import Control.Exception
import Data.Function
import March (march)
import Linear
import System.Environment
import Text.Printf

main :: IO ()
main = do
  (a, b, n) <- catch
    do
      [a0 :: Double, a1, b0, b1, points] <- fmap read <$> getArgs
      pure (V2 a0 a1, V2 b0 b1, floor points)
    do
      \(_ :: SomeException) -> do
        let defa = V2 0.2 0.8
            defb = V2 0.8 0.2
            points = 5
        printf
          do
            "problem gettings arguments; using defaults of (start) %s \
            \and (direction) %s and %d (points)\n\n"
          do show defa
          do show defb
          do points
        pure (defa, defb, points)
  printf " time  ... point\n"
  take n (march a b) & mapM_ \(t, V2 x y) ->
    printf "%.4f ... V2 %.4f %.4f\n" t x y
