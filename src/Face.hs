-- | a mysterious module that apparently samples points on the face of a coid
module Face (facepoints, dbgcountfacepoints, dbgdesmos) where

import Control.Lens hiding (index)
import Data.Functor.Rep
import Data.Ix (range)
import Data.List (intercalate)
import Linear.V3
import Text.Printf

-- | retrieve the points on the faces of a cuboid
-- normal to a certain direction (sig: {-1, 0, 1}) from said cuboid
facepoints :: V3 Int -> V3 Int -> [V3 Int]
facepoints coid sig =
  let (!) = index
      -- p, q, r: dimensions
      -- ps: list of points to sample (?)
      -- h: permutation of a V3
      rule p q r ps h
        | sig ! p == 0 = []
        | otherwise =
            [ let a = if i >= coid ! r then coid ! r else i
                  b = if j >= coid ! q then coid ! q else j
                  c = if sig ! p < 0 then 0 else coid ! p
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
         in range ((si, sj), (coid ! ez - ei, coid ! ey - ej))
      p1 =
        let (sj, ej) = st ez
         in range ((sj, 0), (coid ! ez - ej, coid ! ex))
      p2 = range ((0, 0), (coid ! ey, coid ! ex))
   in concat
        [ rule ex ey ez p0 h0,
          rule ey ex ez p1 h1,
          rule ez ex ey p2 h2
        ]

-- | print a list of vectors in a format that can be copy-pasted into Desmos
dbgdesmos :: (Show a) => [V3 a] -> String
dbgdesmos vs = "[" ++ intercalate "," (f <$> vs) ++ "]"
  where
    f (V3 x y z) = printf "(%s,%s,%s)" (show x) (show y) (show z)

-- | predict the number of points sampled by 'facepoints'
dbgcountfacepoints :: V3 Int -> V3 Int -> Int
dbgcountfacepoints ((+ pure 1) -> coid) sig =
  let (!) = index
      c ? a = if c then a else 0
      allnz3 = all (/= 0)
      allnz2 = all (/= 0)
      V3 cx cy cz = coid
   in (sig ! ex /= 0) ? (cy * cz)
        + (sig ! ey /= 0) ? (cz * cx)
        + (sig ! ez /= 0) ? (cx * cy)
        + if
          | allnz3 sig -> 1 - cx - cy - cz
          | allnz2 $ sig ^. _xy -> -cz
          | allnz2 $ sig ^. _yz -> -cx
          | allnz2 $ sig ^. _zx -> -cy
          | otherwise -> 0
