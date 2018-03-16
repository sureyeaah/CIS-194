import CodeWorld
-- exercise 1
frame = rectangle 2.5 8.0

circleLight c x y = colored c (translated x y (solidCircle 1))
botcircleLight c = circleLight c 0 (-2.0)
midcircleLight c = circleLight c 0 0
topcircleLight c = circleLight c 0 2.0
trafficLight col1 col2 col3 = (botcircleLight col1) & (midcircleLight col2) & (topcircleLight col3) & frame

trafficController :: Double -> Picture
trafficController t
 | round(t) `mod` 10 < 4 = trafficLight green black black
 | round(t) `mod` 10 < 5 = trafficLight black yellow black
 | round(t) `mod` 10 < 9 = trafficLight black black red
 | otherwise = trafficLight black yellow red

exercise1 :: IO()
exercise1 = animationOf trafficController

-- exercise 2
blossom :: Double -> Picture
blossom t = colored yellow (solidCircle (min 0.2 (0.02*t)))

tree :: Picture -> Integer -> Picture
tree s 0 = s
tree s n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree s (n-1)) & rotated (- pi/10) (tree s (n-1)))

blossomingTree :: Double -> Picture
blossomingTree t = tree (blossom t) 8

exercise2 :: IO()
exercise2 = animationOf blossomingTree

-- exercise 3
tile :: Color -> Picture
tile c = colored c (solidRectangle 1 1)

box,wall,ground,storage :: Picture
box = tile brown
wall = tile (gray 0.5)
ground = tile yellow
storage = colored pink (solidCircle 0.5) & ground

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawRow :: Integer -> Integer -> Picture
drawRow _ (-11) = blank
drawRow row col = translated (fromIntegral row) (fromIntegral col) (drawTile (maze row col)) & (drawRow row (col-1))

drawMaze :: Integer -> Picture
drawMaze (-11) = blank
drawMaze row = drawRow row 10 & drawMaze (row-1)

pictureOfMaze :: Picture
pictureOfMaze = drawMaze 10

exercise3 :: IO()
exercise3 = drawingOf pictureOfMaze

-- main 	
main::IO()
main = exercise2