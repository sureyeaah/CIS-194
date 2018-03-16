{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
-- player state
data State = S Coord Direction
initState = S (C 0 1) R

changeState :: Direction -> State -> State
changeState dir (S c _)
 | tileClear (maze (adjCoord dir c))= S (adjCoord dir c) dir
 | otherwise = S c dir
-- player
player :: Direction -> Picture
player dir = head dir & hands dir & rest dir
  where
    head :: Direction -> Picture
    head D = translated 0 (0.5-0.125) (solidCircle 0.125)
    head U = translated 0 (0.5-0.125) (circle 0.125)
    head L = translated 0 (0.5-0.125) (rotated (3*pi/2) (sector 0 pi 0.125 & circle 0.125))
    head R = translated 0 (0.5-0.125) (rotated (pi/2) (sector 0 pi 0.125  & circle 0.125))
    rest :: Direction -> Picture
    rest _ = path [(0,(-0.5)),(0,0.25)]
    hands :: Direction -> Picture
    hands R = (path[(0,0),(0.5,0)]) & (rotated (pi/6) (path[(0,0),(0.5,0)]))
    hands U = (rotated (pi/6) (path[(0,0),(0.5,0)])) & (rotated (5*pi/6) (path[(0,0),(0.5,0)]))
    hands L = (rotated pi (path[(0,0),(0.5,0)])) & (rotated (5*pi/6) (path[(0,0),(0.5,0)]))
    hands D = (rotated (pi/6) (path[(0,0),(0.5,0)])) & (rotated (5*pi/6) (path[(0,0),(0.5,0)]))

-- tiles
maketile :: Color -> Picture
maketile c = colored c (solidRectangle 1 1)

box,wall,ground,storage :: Picture
box = maketile brown
wall = maketile (gray 0.5)
ground = maketile yellow
storage = colored pink (solidCircle 0.2) & ground

data Tile = Wall | Box | Ground | Storage | Blank
drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Box = box
drawTile Ground = ground
drawTile Storage = storage
drawTile Blank = blank

maze :: Coord -> Tile
maze (C x y)
  | x == 0 && y == 0 = Box
  | abs (x+y) > 8 || abs (x-y) > 8  = Blank
  | abs (x+y) == 8 || abs (x-y) == 8 = Wall
  | abs x == 2 || abs y == 2 = Storage
  | otherwise                = Ground

tileClear :: Tile -> Bool
tileClear Storage = True
tileClear Ground = True
tileClear Box = False
tileClear Wall = False
-- Direction
data Direction = R | L | U | D
-- Coordinates
data Coord = C Integer Integer

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjCoord :: Direction -> Coord -> Coord
adjCoord R (C x y) = C (x+1) y
adjCoord L (C x y) = C (x-1) y
adjCoord U (C x y) = C x (y+1)
adjCoord D (C x y) = C x (y-1)


-- drawing maze
drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (maze c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\row -> draw21times (\col -> drawTileAt (C row col)))

-- Interaction

handleTime :: Double -> State -> State
handleTime _ s = s

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = changeState R s
    | key == "Left" = changeState L s
    | key == "Up" = changeState U s
    | key == "Down" = changeState D s
    | otherwise     = s
handleEvent _ s = s

drawState :: State -> Picture
drawState (S c dir) = (atCoord c (player dir)) & pictureOfMaze

resetableInteractionOf ::
    world -> (Double -> world -> world) ->
    (Event -> world -> world) -> (world -> Picture) ->
    IO ()
resetableInteractionOf initial timing handle draw
  = interactionOf initial timing handle' draw
  where
    handle' (KeyPress key) s
      | key == "Esc" = initial
    handle' e s = handle e s 

main :: IO()
main = resetableInteractionOf initState handleTime handleEvent drawState
