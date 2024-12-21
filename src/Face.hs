-- | a mysterious module that apparently samples points on the face of a cube
module Face (facepoints, dbgcountfacepoints) where

import Control.Lens hiding (index)
import Data.Functor.Rep
import Data.Ix (range)
import Linear.V3

-- | apparently this routine samples some points on the face of a cube
-- to test for collisions, idk
facepoints :: V3 Int -> V3 Int -> [V3 Int]
facepoints cube sig =
  let (!) = index
      -- p, q, r: dimensions
      -- ps: list of points to sample (?)
      -- h: permutation of a V3
      rule p q r ps h
        | sig ! p == 0 = []
        | otherwise =
            [ let a = if i >= cube ! r then cube ! r else i
                  b = if j >= cube ! q then cube ! q else j
                  c = if sig ! p < 0 then 0 else cube ! p
               in h do V3 a b c
            | (i, j) <- ps
            ]
      -- this thing is a total mystery to me
      st i -- compute starting / ending points for the face (?) in dimension i
        | sig ! i < 0 = (1, 0)
        | sig ! i > 0 = (0, 1)
        | otherwise = (0, 0)
      h0 (V3 a b c) = V3 c b a
      h1 (V3 a b c) = V3 b c a
      h2 (V3 a b c) = V3 b a c
      p0 =
        let (sj, ej) = st ey
            (si, ei) = st ez
         in range ((si, sj), (cube ! ez - ei, cube ! ey - ej))
      p1 =
        let (sj, ej) = st ez
         in range ((sj, 0), (cube ! ez - ej, cube ! ex))
      p2 = range ((0, 0), (cube ! ey, cube ! ex))
   in concat
        [ rule ex ey ez p0 h0,
          rule ey ex ez p1 h1,
          rule ez ex ey p2 h2
        ]

-- | predict the number of points sampled by 'facepoints'
dbgcountfacepoints :: V3 Int -> V3 Int -> Int
dbgcountfacepoints ((+ pure 1) -> cube) sig =
  let (!) = index
      bnz c a = if c then a else 0
      allnz3 = all (/= 0)
      allnz2 = all (/= 0)
      V3 cx cy cz = cube
   in bnz (sig ! ex /= 0) (cy * cz)
        + bnz (sig ! ey /= 0) (cz * cx)
        + bnz (sig ! ez /= 0) (cx * cy)
        + if
          | allnz3 sig -> 1 - cx - cy - cz
          | allnz2 $ sig ^. _xy -> -cz
          | allnz2 $ sig ^. _yz -> -cx
          | allnz2 $ sig ^. _zx -> -cy
          | otherwise -> 0
