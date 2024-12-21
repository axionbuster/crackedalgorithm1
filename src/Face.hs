-- | a mysterious module that apparently samples points on the face of a cube
module Face (facepoints) where

import Data.Functor.Rep
import Data.Ix (range)
import Linear hiding (ei, ej)

-- | apparently this routine samples some points on the face of a cube
-- to test for collisions, idk
facepoints :: V3 Int -> V3 Int -> V3 Int -> [V3 Int]
facepoints bounding cube sig =
  let (!) = index
      rule ::
        -- dimension 0
        E V3 ->
        -- dimension 1
        E V3 ->
        -- dimension 2
        E V3 ->
        -- points to sample
        [(Int, Int)] ->
        -- permutation
        (V3 Int -> V3 Int) ->
        -- the points
        [V3 Int]
      rule p q r ps h
        | sig ! p == 0 = []
        | otherwise =
             [ let a = if i >= cube ! r then cube ! r else i
                   b = if j >= cube ! q then cube ! q else j
                   c = if sig ! p < 0 then 0 else cube ! p
                in h (V3 a b c) + bounding
              | (i, j) <- ps
              ]
      -- this thing is a total mystery to me
      h0 (V3 a b c) = V3 c b a
      h1 (V3 a b c) = V3 b c a
      h2 (V3 a b c) = V3 b a c
      p0 =
        let (sj, ej)
              | sig ! ey < 0 = (1, 0)
              | sig ! ey > 0 = (0, 1)
              | otherwise = (0, 0)
            (si, ei)
              | sig ! ez < 0 = (1, 0)
              | sig ! ez > 0 = (0, 1)
              | otherwise = (0, 0)
         in range ((si, sj), (cube ! ez - ei, cube ! ey - ej))
      p1 =
        let (sj, ej)
              | sig ! ez < 0 = (1, 0)
              | sig ! ez > 0 = (0, 1)
              | otherwise = (0, 0)
         in range ((sj, 0), (cube ! ez - ej, cube ! ex))
      p2 = range ((0, 0), (cube ! ey, cube ! ex))
   in concat
        [ rule ex ey ez p0 h0,
          rule ey ex ez p1 h1,
          rule ez ex ey p2 h2
        ]
