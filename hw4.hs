{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- list related functions
data List a = Empty | Entry a (List a) deriving Eq
mapList :: (a -> t) -> List a -> List t
mapList _ Empty = Empty
mapList f (Entry c rest) = Entry (f c) (mapList f rest)
appendList :: List a -> List a -> List a
appendList Empty list = list
appendList (Entry c rest) list = Entry c (appendList rest list)
combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & (combine ps)
elemList :: Eq a => a -> List a -> Bool
elemList x Empty = False
elemList x (Entry c rest) = (x == c) || elemList x rest
listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry _ rest) = 1 + (listLength rest)
filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Entry c rest)
    | (p c) = Entry c (filterList p rest)
    | otherwise = filterList p rest
nth :: List a -> Integer -> a
nth Empty _ = error "list too short"
nth (Entry c rest) n
    | n==1 = c
    | otherwise = nth rest (n-1)
allTrue :: List Bool -> Bool
allTrue Empty = True
allTrue (Entry x rest) = x && (allTrue rest)
-- Direction
data Direction = R | L | U | D deriving Eq
directions :: List Direction
directions = Entry R (Entry L (Entry U (Entry D Empty)))
-- Coordinates
data Coord = C Integer Integer
instance Eq Coord where
    C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
adjCoord :: Direction -> Coord -> Coord
adjCoord R (C x y) = C (x+1) y
adjCoord L (C x y) = C (x-1) y
adjCoord U (C x y) = C x (y+1)
adjCoord D (C x y) = C x (y-1)

-- Game screen state
data SSState world = StartScreen | Running world
instance Eq s => Eq (SSState s) where
    StartScreen == StartScreen = True
    Running s == Running s' = s == s'
    _ == _ = False
-- game state
data State = S Coord Direction (List Coord) Integer deriving Eq
generateState :: Integer -> State
generateState level = S initCoord R (initialBoxes (nthMaze level)) level
    where initCoord = case nth mazes level of (Maze x _) -> x

initState :: State
initState = generateState 1

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
data Tile = Wall | Box | Ground | Storage | Blank deriving Eq
drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Box = box
drawTile Ground = ground
drawTile Storage = storage
drawTile Blank = blank

-- checking if maze if correct
isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (Entry initial Empty)
    where
        go _ Empty = True
        go seen (Entry c rest)
            | elemList c seen = go seen rest
            | not (isOk c) = False
            | otherwise = go (Entry c seen) (appendList rest (adjacent c))
isClosed :: Maze -> Bool
isClosed (Maze init graph) = initOK && (isGraphClosed init adjacent (\c -> graph c /= Blank))
    where
        initOK = case graph init of
            Ground -> True
            Storage -> True
            _ -> False
        adjacent x = filterList (\c -> graph c /= Wall) (mapList (\d -> adjCoord d x) directions)
-- maze
nthMaze :: Integer -> (Coord -> Tile) 
nthMaze n = case nth mazes n of (Maze _ maze) -> maze
noBoxMaze :: (Coord -> Tile) -> Coord -> Tile
noBoxMaze maze c = case maze c of
    Box -> Ground
    t -> t
mazeWithBoxes :: (Coord -> Tile) -> List Coord -> Coord -> Tile
mazeWithBoxes maze Empty x = noBoxMaze maze x
mazeWithBoxes maze (Entry c rest) x
    | c == x = Box
    | otherwise = mazeWithBoxes maze rest x
-- boxes
initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes maze = appendList (go (\r -> (go (\c -> checkBox (C r c)) (-10))) (-10)) Empty
    where
        go _ 11 = Empty
        go f n = appendList (f n) (go f (n+1)) 
        checkBox :: Coord -> List Coord
        checkBox c = case maze c of 
            Box -> (Entry c Empty)
            t -> Empty
pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes locations = combine (mapList (\c -> atCoord c (drawTile Box)) locations)
-- drawing maze
drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt maze c = atCoord c (drawTile (noBoxMaze maze c))
draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
    where
        go :: Integer -> Picture
        go 11 = blank
        go n = something n & go (n+1)
pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze maze = draw21times (\row -> draw21times (\col -> drawTileAt maze (C row col)))
-- Winning
isWon :: State -> Bool
isWon (S _ _ boxes level) = allTrue (mapList isOnStorage boxes)
    where
        isOnStorage :: Coord -> Bool
        isOnStorage c = case noBoxMaze (nthMaze level) c of
            Storage -> True
            t -> False
winMessage :: Integer -> Picture
winMessage level
    | level == (listLength mazes)= scaled 1 1 (text "You won!")
    | otherwise = scaled 1 1 (text "Level Complete!")
-- undo
data WithUndo a = WithUndo a (List a)
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw)
    =  Interaction state0' step' handle' draw'
    where
        state0' = WithUndo state0 Empty
        step' t (WithUndo s stack) = WithUndo (step t s) stack
        handle' (KeyPress key) (WithUndo s stack) | key == "U"
            = case stack of Entry s' stack' -> WithUndo s' stack'
                            Empty           -> WithUndo s Empty
        handle' e (WithUndo s stack)
            | s' == s = WithUndo s stack
            | otherwise = WithUndo (handle e s) (Entry s stack)
            where s' = handle e s
        draw' (WithUndo s _) = draw s
-- Interaction
data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)    
handleTime :: Double -> State -> State
handleTime _ s = s
changeState :: Direction -> State -> State
changeState dir (S from _ boxes level) = case mazeWithBoxes (nthMaze level) boxes to of
    Ground -> freeMove
    Storage -> freeMove
    Box -> case mazeWithBoxes (nthMaze level) boxes oneFurther of
        Ground -> boxMoved
        Storage -> boxMoved
        _ -> noMove
    _ -> noMove
    where
        to = (adjCoord dir from)
        oneFurther = (adjCoord dir to)
        freeMove = S to dir boxes level
        noMove = S from dir boxes level
        boxMoved = S to dir (mapList changeBox boxes) level
        changeBox c
             | c == to   = oneFurther
             | otherwise = c
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | isWon s && level /= (listLength mazes)= generateState (level+1)
    | isWon s = s
    | key == "Right" = changeState R s
    | key == "Left" = changeState L s
    | key == "Up" = changeState U s
    | key == "Down" = changeState D s
        where level = case s of (S _ _ _ x) -> x
handleEvent _ s = s
drawState :: State -> Picture
drawState (S c dir boxCoords level)
    | isWon (S c dir boxCoords level) = (winMessage level) & (atCoord c (player dir))  & (pictureOfBoxes boxCoords) & pictureOfMaze (nthMaze level)
    | otherwise = (atCoord c (player dir))  & (pictureOfBoxes boxCoords) & pictureOfMaze (nthMaze level)
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
sokoban :: Interaction State
sokoban = Interaction initState (\ _ c -> c) handleEvent drawState 
main :: IO()
main = runInteraction (withUndo (resetable (withStartScreen sokoban)))

-- Mazes
data Maze = Maze Coord (Coord -> Tile) 
mazes :: List Maze
mazes =
  Entry (Maze (C 1 1)       maze9) $
  Entry (Maze (C 0 0)       maze8) $
  Entry (Maze (C (-3) 3)    maze7) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 0 1)       maze1) $
  Empty
extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3))    maze4')  $
  Entry (Maze (C 1 (-3))    maze4'') $
  Entry (Maze (C 1 1)       maze9') $
  mazes
maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall
maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall
maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall
maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall
maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall
maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall
maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall
maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall
maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall
maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall
maze3 _ = Blank
maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground
maze5 :: Coord -> Tile 
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground
maze6 :: Coord -> Tile 
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground
maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground
maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground
maze9 :: Coord -> Tile 
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground
maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c
maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c
maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c
