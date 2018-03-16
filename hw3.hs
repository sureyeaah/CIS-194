{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- some data structures and generic functions
data List a = Empty | Entry a (List a)
mapList :: (a -> t) -> List a -> List t
mapList _ Empty = Empty
mapList f (Entry c rest) = Entry (f c) (mapList f rest)
appendList :: List a -> List a -> List a
appendList Empty list = list
appendList (Entry c rest) list = appendList rest (Entry c list)
combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & (combine ps)

-- Direction
data Direction = R | L | U | D

-- Coordinates
data Coord = C Integer Integer
eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
adjCoord :: Direction -> Coord -> Coord
adjCoord R (C x y) = C (x+1) y
adjCoord L (C x y) = C (x-1) y
adjCoord U (C x y) = C x (y+1)
adjCoord D (C x y) = C x (y-1)

-- Game screen state
data SSState world = StartScreen | Running world

-- game state
data State = S Coord Direction (List Coord)
initState :: State
initState = S (C 2 2) R initialBoxes

-- start screen
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

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

-- maze
maze :: Coord -> Tile
maze (C x y)
    | x == 0 && y == 0 = Box
    | x == 0 && y == 1 = Box
    | abs (x+y) > 8 || abs (x-y) > 8  = Blank
    | abs (x+y) == 8 || abs (x-y) == 8 = Wall
    | x == 1 && y == 0 = Storage
    | x == 1 && y == 1 = Storage
    | otherwise                = Ground
noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
    Box -> Ground
    t -> t
mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty x = noBoxMaze x
mazeWithBoxes (Entry c rest) x
    | c `eqCoord` x = Box
    | otherwise = mazeWithBoxes rest x

-- boxes
initialBoxes :: List Coord
initialBoxes = appendList (go (\r -> (go (\c -> checkBox (C r c)) (-10))) (-10)) Empty
    where
        go _ 11 = Empty
        go f n = appendList (f n) (go f (n+1)) 
        checkBox :: Coord -> List Coord
        checkBox c = case maze c of 
            Box -> (Entry c Empty)
            t -> Empty
pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes locations = combine (mapList (\c -> atCoord c (drawTile Box)) locations)
movingBoxes :: Interaction (List Coord)
movingBoxes = Interaction initialBoxes (\_ s -> s) handle draw
    where
        draw = pictureOfBoxes
        handle (KeyPress key) s
            | key == "Right" = mapList (adjCoord R) s
            | key == "Left"  = mapList (adjCoord L) s
            | key == "Up"    = mapList (adjCoord U) s
            | key == "Down"  = mapList (adjCoord D) s
        handle _ s      = s

-- drawing maze
drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))
draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n = something n & go (n+1)
pictureOfMaze :: Picture
pictureOfMaze = draw21times (\row -> draw21times (\col -> drawTileAt (C row col)))
-- Winning
isWon :: State -> Bool
isWon (S _ _ boxes) = allTrue (mapList isOnStorage boxes)
isOnStorage :: Coord -> Bool
isOnStorage c = case noBoxMaze c of
    Storage -> True
    t -> False
allTrue :: List Bool -> Bool
allTrue Empty = True
allTrue (Entry x rest) = x && (allTrue rest)
winMessage :: Picture
winMessage = scaled 3 3 (text "You won!")
-- Interaction
data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)    
handleTime :: Double -> State -> State
handleTime _ s = s
changeState :: Direction -> State -> State
changeState dir (S from _ boxes) = case mazeWithBoxes boxes to of
    Ground -> freeMove
    Storage -> freeMove
    Box -> case mazeWithBoxes boxes oneFurther of
        Ground -> boxMoved
        Storage -> boxMoved
        _ -> noMove
    _ -> noMove
    where
        to = (adjCoord dir from)
        oneFurther = (adjCoord dir to)
        freeMove = S to dir boxes
        noMove = S from dir boxes
        boxMoved = S to dir (mapList changeBox boxes)
        changeBox c
             | c `eqCoord` to   = oneFurther
             | otherwise = c
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | isWon s = s
    | key == "Right" = changeState R s
    | key == "Left" = changeState L s
    | key == "Up" = changeState U s
    | key == "Down" = changeState D s
handleEvent _ s = s
drawState :: State -> Picture
drawState (S c dir boxCoords)
    | isWon (S c dir boxCoords) = winMessage
    | otherwise = (atCoord c (player dir))  & (pictureOfBoxes boxCoords) & pictureOfMaze
resetableInteractionOf ::
    world -> (Double -> world -> world) ->
    (Event -> world -> world) -> (world -> Picture) ->
    IO ()
resetableInteractionOf state0 step handle draw
  = interactionOf state0 step handle' draw
  where
    handle' (KeyPress key) s
      | key == "Esc" = state0
    handle' e s = handle e s 
resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s
withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
runInteraction :: Interaction s-> IO()
runInteraction (Interaction state0 step handle draw)
    = interactionOf state0 step handle draw
game :: Interaction State
game = Interaction initState (\ _ c -> c) handleEvent drawState 
main :: IO()
main = runInteraction (resetable (withStartScreen game))
