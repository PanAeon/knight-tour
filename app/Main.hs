module Main where

import Lib

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1 # fc blue
                    # lw veryThick
                    # lc purple
                    # dashingG [0.2,0.05] 0

example :: Diagram B
example = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none

example1 :: Diagram B
example1 =  square 1 # fc aqua `atop` circle 1


circles :: Diagram B
circles = hcat (map circle [1..6])

example2 :: Diagram B
example2 = vcat (replicate 3 circles)

boardSize = 6 -- FIXME:: !!!!

result :: [(Int, Int)]
result=[(6,2),(7,3),(8,0),(8,4),(9,1),(9,5),(10,2),(12,1),(13,0),(14,6),(15,11),(16,3),(16,5),(17,4),(18,7),(19,15),(20,12),(22,11),(23,10),(24,13),(24,20),(25,14),(28,17),(29,21),(30,19),(30,26),(31,18),(31,27),(32,21),(32,28),(33,25),(33,29),(34,23),(34,26),(35,22),(35,27)]
unpackCoords k n = (k `mod` n, k `div` n)

translateCoords (x,y) = (fromIntegral x - hw, fromIntegral y - 1.5)

hw = (fromIntegral (boardSize `div` 2)) + 0.5

l' :: Int
l' = -1 - (boardSize `div` 2)
h' :: Int
h' = -1 + (boardSize `div` 2)

-- https://www.w3.org/TR/SVG11/types.html#ColorKeywords
checkboard :: Diagram B
checkboard = vcat (replicate boardSize (hcat (replicate boardSize  sq )))  # translate (r2 (-hw, hw)) -- # showOrigin
  where
    sq = square 1 #fc blanchedalmond # lw thick #lc black

node :: Diagram B
node    = circle 0.1 # fc green

--foobar = position --atPoints (trailVertices $ regPoly 6 1) (repeat node)
foobar :: Diagram B
foobar = position (zip (map p2 [((fromIntegral i)+0.5,(fromIntegral j)+0.5) | i <- [l'..h'], j <- [l'..h']]) (repeat node))




produceLines :: Diagram B
produceLines = mconcat ls
  where
    xs = map foo result
    foo (a,b) = (translateCoords (unpackCoords a boardSize), translateCoords (unpackCoords b boardSize))
    ls = map bar xs
    bar (a,b) = exLine (r2 a) (r2 b)

-- fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lwO 20

-- illustrateBézier:: V2 Double -> V2 Double -> V2 Double -> Diagram B
-- illustrateBézier c1 c2 x2
--      =  endpt
--      <> endpt  # translate x2
--      <> ctrlpt # translate c1
--      <> ctrlpt # translate c2
--      <> l1
--      <> l2
--      <> fromSegments [bézier3 c1 c2 x2]
--    where
--      dashed  = dashingN [0.03,0.03] 0
--      endpt   = circle 0.05 # fc red  # lw none
--      ctrlpt  = circle 0.05 # fc blue # lw none
--      l1      = fromOffsets [c1] # dashed
--      l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed
--
-- x2      = r2 (3,-1) :: V2 Double     -- endpoint
-- [c1,c2] = map r2 [(1,2), (3,0)]     -- control points
--
-- exampleB :: Diagram B
-- exampleB = illustrateBézier c1 c2 x2

exLine :: V2 Double -> V2 Double -> Diagram B
exLine p1 p2 = (endpt # translate p1) <> (endpt # translate p2)  <> (fromOffsets [p1 ^-^ p2] # translate p2 # lw 1.5 # lc red) -- (fromVertices [p1, p2] # lwO 0.3 # lc red)
  where
    endpt   = circle 0.1 # fc red  # lw none

main :: IO ()
main = mainWith ({-exLine (r2 (-3.5,-3.5)) (r2 (2.5,1.5))-} produceLines `atop` checkboard ) -- r2 && p2
