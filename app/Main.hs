module Main (main) where

import Control.Exception
import Data.Function
import Linear
import March (March (..), march)
import System.Environment
import Text.Printf

main :: IO ()
main = do
  (a, b, n) <- catch
    do
      [a0 :: Double, a1, a2, b0, b1, b2, points] <- fmap read <$> getArgs
      pure (V3 a0 a1 a2, V3 b0 b1 b2, floor points)
    do
      \(_ :: SomeException) -> do
        let defa = V3 0.2 0.8 0.5
            defb = V3 0.8 0.2 0.5
            points = 10
        printf
          do
            "problem gettings arguments; using defaults of (start) %s \
            \and (direction) %s and %d (points)\n\n"
          do show defa
          do show defb
          do points
        pure (defa, defb, points)
  printf " time  ... point\n"
  take n (march a b) & mapM_ \(March t (V3 x y z) blocks) ->
    printf "%.4f ... V3 %.4f %.4f %.4f %s\n" t x y z (show blocks)
