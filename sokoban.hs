{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Direction = R | U | L | D | N deriving (Eq)
data Coord = C Integer Integer deriving (Eq)
data State = S {
  stPlayer :: Coord,
  stDir    :: Direction,
  stBoxes  :: [Coord],
  stMap    :: Coord -> Tile,
  stLevel  :: Integer,
  stSteps  :: Integer
}
instance Eq State where
  S p d b _ l s == S p' d' b' _ l' s' = 
    (p == p' && d == d' && b == b' && l == l' && s == s')
data SSState world = StartScreen | Running world deriving (Eq)
data Activity world = Activity {
    actState  :: world,
    actHandle :: (Event -> world -> world),
    actDraw   :: (world -> Picture)
}
data WithUndo a = WithUndo a [a] deriving (Eq)
data Resettable a = Resettable a a deriving (Eq)
data Maze = Maze Coord (Coord -> Tile)


main :: IO ()
main = etap5


etap5 :: IO ()
etap5 = walk (Activity (initialState 0) handleEvent draw)


walk :: Eq s => Activity s -> IO()
walk activity = runActivity $ withStartScreen $ resettable $ withUndo activity


runActivity :: Activity s -> IO ()
runActivity (Activity state handle draw) = activityOf state handle draw


withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' 
  where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s


withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


resettable :: Activity s -> Activity (Resettable s)
resettable (Activity state0 handle draw) = Activity state0' handle' draw'
  where 
    state0' = Resettable state0 state0
  
    handle' (KeyPress key) (Resettable _ prev) | key == "Esc"
      = (Resettable prev prev)
    handle' e (Resettable s prev)
      | e == (KeyPress "N") = (Resettable (handle e s) s')
      | otherwise = (Resettable (handle e s) prev)
      where s' = handle e s
    
    draw' (Resettable s _) = draw s


startScreenActivityOf ::
    world ->
    (Event -> world -> world) -> 
    (world -> Picture) -> IO ()
startScreenActivityOf state0 handle draw
  = activityOf state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


startScreen :: Picture
startScreen = translated 0 2 
  (scaled 3 3 (lettering "Sokoban!") & translated 0 (-6) etap4)
  

etap4 :: Picture
etap4 = pictureOfBools (mapList (\x -> (isClosed x) && (isSane x)) 
  (reverse (appendList mazes badMazes)))


winningScreen :: State -> Picture
winningScreen state = scaled 1 1 (
  translated 0 2 (lettering (T.pack ("Level " ++ (show (stLevel state)) ++ " finished"))) &
  (lettering (T.pack ("Number of steps: " ++ (show (stSteps state)) ++ "\n"))) &
  translated 0 (-2) (lettering (T.pack ("Press 'N' to get to the next level."))))


isWinning :: State -> Bool
isWinning state = not failed
  where
    (failed, _) = boxesOnStorage (stPlayer state) False (\x -> False)
    level = stMap state
    neighbours c@(C x y) = if level c == Wall || level c == Blank then [] 
      else [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
    boxesOnStorage v acc visited = 
      foldList (\e ac -> f e ac) (acc, visited) (neighbours v)
      where
        f v' (acc', visited') =
          if visited' v' then (acc', visited')
          else
            let (newacc, newv) = boxesOnStorage v' acc' (enrich visited' [v']) in
              if elem v' (stBoxes state) then (newacc || not (level v' == Storage), newv)
              else (newacc, newv)


initialState :: Integer -> State
initialState nr = S initial U (initialBoxes maze) (removeBoxes level) nr  0
  where
    maze@(Maze initial level) = if nr >= toInteger(length mazes) then head mazes else nth mazes nr

  
initialBoxes :: Maze -> [Coord]
initialBoxes (Maze initial level) = boxes
  where
    (boxes, _) = scrapeBoxes initial [] (\x -> False) 
    neighbours c@(C x y) = if level c == Wall || level c == Blank then [] 
      else [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
    scrapeBoxes v acc visited =
      foldList (\e ac -> f e ac) (acc, visited) (neighbours v) 
      where 
        f v' (acc', visited') =
          if visited' v' then (acc', visited') 
          else
            let (newacc, newv) = scrapeBoxes v' acc' (enrich visited' [v']) in
              if level v' == Box then (v':newacc, newv)
              else (newacc, newv)


removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes level = f . level 
  where 
    f Box = Ground
    f a = a


draw :: State -> Picture
draw state =
  if isWinning state then 
    winningScreen state
  else
    (translated (fromIntegral xP) (fromIntegral yP) (player (stDir state)))
    & pictures [translated (fromInteger x) (fromInteger y) (drawTile Box) 
      | (C x y) <- stBoxes state]
    & pictures [translated (fromInteger x) (fromInteger y) 
      (drawTile (stMap state (C x y))) 
      | x <- [-maxRange..maxRange], y <- [-maxRange..maxRange]]
  where
    C xP yP = stPlayer state
    maxRange = getRange (stMap state) 10
    getRange level range = 
      if (allList (\x -> (level x) == Blank) (appendList
          [(C x y) | x <- [-range, range], y <- [-range..range]]
          [(C x y) | y <- [-range, range], x <- [-range..range]])) then range
      else getRange level (range + (range `div` 2))


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state =
    if key == "N" then initialState (if ((stLevel state) + 1) >= toInteger(length mazes) then 0 else (stLevel state) + 1)
    else if isWinning state then state
    else if direction == N then state
    else if canMove direction state then
      state {stPlayer = adjacentCoord direction (stPlayer state),
        stBoxes = movedBoxes (stPlayer state) direction (stBoxes state),
        stDir = direction, stSteps = (stSteps state + 1)
      }
    else
      state {stDir = direction}
  where
    direction = 
      if key == "Right" || key == "D" then R
      else if key == "Up" || key == "W" then U
      else if key == "Left" || key == "A" then L
      else if key == "Down" || key == "S" then D
      else N
    canMove :: Direction -> State -> Bool
    canMove d s = (field == Ground || field == Storage) &&
      not (elem (C x y) (stBoxes s) 
        && (nextField == Wall || elem (C x1 y1) (stBoxes s))) 
      where
        field = (stMap state) (C x y)
        nextField = (stMap state) (C x1 y1)
        C x y = adjacentCoord d (stPlayer state)
        C x1 y1 = adjacentCoord d (C x y)
    movedBoxes :: Coord -> Direction -> [Coord] -> [Coord]
    movedBoxes pos d boxes = map f boxes
      where
        f :: Coord -> Coord
        f c = if c == adj then adjacentCoord d c else c
          where adj = adjacentCoord d pos

handleEvent _ state = state


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)
adjacentCoord N (C x y) = C x y


player :: Direction -> Picture
player dir
  | dir == N = blank
  | dir == U = lightning & playerFront
  | dir == D = translated 0 0.2 lightning & playerBack
  | dir == R = translated (-0.1) 0.1 lightning & playerRight
  | dir == L = translated 0.08 0.1 lightning & playerLeft
  where
    playerFront = translated 0 (-0.1)
      (playerBase  & (playerShadow playerBase))
    playerBack = translated 0 0.1
      (shape & (playerShadow shape))
        where shape = rotated (-pi) playerBase
    playerRight = translated (-0.1) 0
      (shape & (playerShadow shape))
        where shape = rotated (-pi/2) playerBase
    playerLeft = translated 0.1 0
      (shape  & (playerShadow shape))
        where shape = rotated (pi/2) playerBase
    lightning = translated (-0.15) (0.08)
      (colored (RGBA 1 1 1 0.2)
        (rotated (-pi/4) (scaled 0.3 0.5 (solidCircle 0.5))))
    playerShadow :: Picture -> Picture
    playerShadow shape = translated 0.1 (-0.1) (colored (RGBA 0 0 0 0.2) shape)
    playerBase = scaled 0.34 0.34 (wings & bugHead & body)
      where
        wings = wing & reflected (pi/2) wing
          where
            wing = scaled 1 1.1 (rotated 0.1
              (translated 0.1 0
                (dots
                  & colored red (sector (-pi/2) (pi/2) 1)
                  & colored black (sector (-pi/2) (pi/2) 1.01))))
            dots = pictures [translated x y dot | (x, y) <-
              [(0.3, 0.6), (0.25, -0.1), (0.45, -0.55), (0.65, 0.2)]]
            dot = colored black (solidCircle 0.2)
        body = colored black (solidCircle 1)
        bugHead = scaled 1.2 1.1
          (eyes
          & antennae
          & translated 0 1.06 (colored black (solidCircle 0.3)))
          where
            eyes = eye & reflected (pi/2) eye
            antennae = antenna & reflected (pi/2) antenna
            eye = translated 0.1 1.3 (
              translated 0 0.02 (
                colored black (solidCircle 0.01))
                & colored white (solidCircle 0.05))
            antenna = translated 0 0.9
              (colored black
                (thickCurve 0.02 [(0.15, 0.15), (0.2, 0.4), (0.3, 0.6)]
                & translated 0.3 0.6 (solidCircle 0.03)))


wall :: Picture
wall = colored lightShadow (rotated (-pi/2) corner)
  & colored shadow (rotated (pi) corner)
  & colored darkShadow (rotated (pi/2) corner)
  & colored shadow corner
  & colored darkShadow (rotated pi cornerShadow)
  & colored lightShadow cornerShadow
  & colored mainColor roundedRectangle
  where
    mainColor = RGBA (99/255) (101/255) (156/255) 1
    darkShadow = RGBA 0 0 (74/255) 1
    shadow = RGBA (49/255) (48/255) (99/255) 1
    lightShadow = RGBA (156/255) (154/255) (206/255) 1
    corner = translated (-0.5) (-0.5) (solidPolygon
      [(0, 1/50), (0, 1/6), (1/50, 1/6), (1/6, 1/50), (1/6, 0), (1/50, 0)])
    cornerShadow = translated 0.01 (-0.01) (thickPolyline 0.02
      [(-1/2, -12/25 + 0.01), (-1/2, 12/25), (-12/25, 1/2), (12/25 - 0.01, 1/2)])
    roundedRectangle = solidPolygon [
      (-1/2, 12/25), (-12/25, 1/2), (12/25, 1/2), (1/2, 12/25),
      (1/2, -12/25), (12/25, -1/2), (-12/25, -1/2), (-1/2, -12/25)]


box :: Picture
box = (rotated (-pi/2) plank) & plank & back & front
  where
    mainColor = RGBA 1 (182/255) (80/255) 1
    shadow = RGBA (218/255) (125/255) 0 1
    lightShadow = RGBA 1 (218/255) (170/255) 1
    plank = colored lightShadow (translated (0.007) (-0.007)
        (thickPolyline 0.01 [(-2/7, -0.2), (0.2, 2/7)]))
      & colored shadow (thickPolyline 0.01
        [(0.21, 2/7 - 0.01), (2/7 - 0.01, 2/7 - 0.01), (2/7 - 0.01, 0.205),
        (-0.195, 0.01 - 2/7), (0.01 - 2/7, 0.01 - 2/7), (0.01 - 2/7, -0.199)])
      & thickPolygon 0.01 boundaries
      & colored mainColor (solidPolygon boundaries)
      where
        boundaries = [(0.2, 2/7), (2/7, 2/7), (2/7, 0.2),
          (-0.2, -2/7), (-2/7, -2/7), (-2/7, -0.2)]
    back = (thickRectangle 0.01 (4/7) (4/7))
      & arcShadow
      & rotated (-pi/2) arcShadow
      & reflected 0 arcShadow
      & reflected (pi/2) (rotated (-pi/2) arcShadow)
      & colored shadow (thickRectangle (1/28) 0.55 0.55)
      & colored mainColor (solidRectangle (4/7) (4/7))
      where
        arcShadow = colored shadow (thickPolyline 0.01
          [(0, 0.095), (-0.16, 0.26), (0.17, 0.26)])
    front = colored shadow innerShadow
      & colored lightShadow (rotated pi innerShadow)
      & colored lightShadow (rotated pi outerShadow)
      & colored shadow outerShadow
      & thickRectangle 0.02 1 1
      & colored mainColor (solidRectangle 1 1)
      where
        outerShadow = thickPolyline 0.02
          [(-0.48, -0.48), (0.48, -0.48), (0.48, 0.48)]
        innerShadow = thickPolyline 0.02
          [(-0.29, -0.295), (0.295, -0.295), (0.295, 0.29)]


ground :: Picture
ground = stripes
  & reflected 0 (reflected (pi/2) stripes)
  & colored ice (solidRectangle 1 1)
  where
    ice = RGBA (206/255) 1 1 1
    stripe = colored white (solidRectangle 0.1 0.02)
    stripes = pictures [translated x y stripe | (x, y) <-
      [(-0.35, 0.4), (-0.1,  0.35), (0.1, 0.4), (0.3, 0.26), (0.4, 0.45),
      (-0.4, 0.3), (-0.2, 0.2), (0,0.2), (0.25, 0.16),
      (-0.45, 0.13), (-0.3, 0.03), (0, 0.08), (0.3, 0.1)]]


storage :: Picture
storage = zone & ground
  where
    zone = pictures [part r | r <- [0.01, 0.1, 0.2, 0.3, 0.4]]
    part r = colored white (thickCircle 0.005 r)
      & colored pink (thickCircle 0.02 r)
      & colored red (thickCircle 0.035 r)


drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = colored black (solidRectangle 1 1)

  
mazes :: [Maze]
mazes = 
  [
    (Maze (C (-1) 0) (
       \(C x y) ->
         if abs x > 2 || abs y > 1 then Blank
         else if x == -1 && y == 0 then Ground
         else if x ==  0 && y == 0 then Box
         else if x ==  1 && y == 0 then Storage
         else                           Wall)),
    (Maze (C 1 1) (
      \(C x y) ->
        if abs x > 4  || abs y > 4       then Blank
        else if abs x == 4 || abs y == 4 then Wall
        else if x ==  2 && y <= 0        then Wall
        else if x ==  3 && y <= 0        then Storage
        else if x >= -2 && y == 0        then Box
        else                                  Ground)),
    (Maze (C 0 (-1)) (
      \(C x y) ->
        if abs x > 4 || abs y > 4            then Blank
        else if abs x == 4 || abs y == 4     then Wall
        else if x == -1 && y >= -1           then Wall
        else if x ==  2 && y <= -1           then Wall
        else if x ==  0 && y == -2           then Storage
        else if x == -2 && y == 0            then Storage
        else if (x == 0 || x == 1) && y == 0 then Box
        else                                      Ground)),
    (Maze (C 0 1) (
      \(C x y) ->
        if abs x > 6 || abs y > 4           then Blank
        else if abs x == 6 || abs y == 4    then Wall
        else if x ==  2 && y <= 0           then Wall
        else if x ==  1 && y <= -2          then Storage
        else if x == -1 && y <= -2          then Storage
        else if x == -3 && y <= -2          then Storage
        else if x >= -3 && y == 0 && x <= 3 then Box
        else                                     Ground)),
    (Maze (C 0 (-2)) (
      \(C x y) ->
        if abs x > 4 || abs y > 3                               then Blank
        else if abs x > 3 || abs y > 2                          then Wall
        else if elem (x, y) 
            [(-3,  2), (-2,  2), (2,  2), (3, 2), 
             (-3, -2), (-2, -2), (2, -2), (3,-2),
             (-2,  0), (0,  -1), (2,  0)  ]                     then Wall
        else if elem (x, y) [(0, 0), (0, 1), (-2, -1), (2, -1)] then Box
        else if y >= 0 && (y == -x - 1 || y == x - 1)            then Storage
        else                                                         Ground)),
    (Maze (C 0 0) (
      \(C x y) ->
        if abs x > 4 || abs y > 4                                then Blank
        else if abs x == 4 || abs y == 4                         then Wall
        else if abs x == 3 && y == 0                             then Wall
        else if abs y == 3 && x == 0                             then Wall
        else if x ==  0    && y == 0                             then Ground
        else if x ==  y    && abs x < 3                          then Storage
        else if x == -y    && abs x < 3                          then Storage
        else if x == -3    && y == -2                            then Ground
        else if (x == 0 || y == 0) && (abs x == 2 || abs y == 2) then Ground
        else if abs x <= 2 && abs y < 2                          then Box
        else                                                          Ground)),
    (Maze (C (-2) (-3)) (
      \(C x y) ->
        if  abs x >  4     || y >  6 || y <  -4              then Blank
        else if x < -2     && y >  3                         then Blank
        else if x >  1     && y >  5                         then Blank
        else if x >  3     && y == 1                         then Blank
        else if abs x == 4 || y == 6 || y == -4                        then Wall
        else if elem (x, y) [
            (-2, 5), (1, 5),  (2, 5), (3, 5),   (-2, 4), 
            (-3, 3), (-2, 3), (0, 3), (1, 3),   (1, 2), 
            (3, 2),  (1, 1),  (3, 1), (-2, 0),  (-1, 0), 
            (0, 0),  (1, 0),  (3, 0), (-2, -1), (-2, -2), 
            (1, -3)  ]                                           then Wall
        else if x < -1   && y > 0                                then Storage
        else if elem (x, y) [(0, -1), (0, -2), (1, -2), (2, -2)] then Box
        else                                                          Ground))
  ]
  

badMazes :: [Maze]
badMazes = 
  [
    (Maze (C 0 0) (\x  -> Blank)),
    (Maze (C 0 0) (\(C x y) -> if ((x + y) `mod` 2) == 1 then Blank else Wall)),
    (Maze (C 1 1) (
      \(C x y) ->
        if abs x > 4  || abs y > 4       then Blank
        else if abs x == 4 || abs y == 4 then Wall
        else if x ==  2 && y <= 0        then Wall
        else if x >= -2 && y == 0        then Box
        else                                  Ground)),
    (Maze (C (-1) 0) (
       \(C x y) ->
         if abs x > 2 || abs y > 1 then Blank
         else if x == -1 && y == 0 then Ground
         else if x ==  0 && y == 0 then Storage
         else if x ==  1 && y == 0 then Box
         else                           Wall)),
    (Maze (C 2 0) (
       \(C x y) ->
         if x < 1 || x > 3 || abs y > 3       then Blank
         else if 1 < x  && x < 4 && abs y > 1 then Blank
         else if x < 1  && y == 0             then Blank
         else if x == 2 && y == 0             then Ground
         else if x == 0 && y == 2             then Storage
         else if x == 0 && y == -2            then Box
         else                                      Wall)),
    (Maze (C 0 (-1)) (
      \(C x y) ->
        if abs x > 4  || abs y > 4           then Blank
        else if abs x == 4 || abs y == 4     then Wall
        else if x == -1 && y >= -1           then Wall
        else if x ==  1 && y <= -1           then Wall
        else if x ==  0 && y == -2           then Storage
        else if x == -2 && y == 0            then Storage
        else if (x == 0 || x == 1) && y == 0 then Box
        else                                      Ground)),
    (Maze (C 0 0) (
      \(C x y) ->
        if abs x > 4  || abs y > 2              then Blank
        else if x >= -1 && x <= 4 && abs y == 1 then Wall
        else if x ==  4 && y == 0               then Wall
        else if x == -1 && y == 0               then Wall
        else if x ==  0 && y == 0               then Ground
        else if x ==  3 && y == 0               then Storage
        else if (x == 1 || x == 2) && y == 0    then Box
        else                                         Blank)),
    (Maze (C 0 0) (\(C x y) -> if x == 0 && y == 0 then Box else Blank)),
    (Maze (C 0 0) (
      \(C x y) ->
        if abs x > 1 || abs y > 1 then Blank
        else if abs x == 1 || y == -1 then Wall
        else Ground))
  ]


foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc []    = acc
foldList fun acc (h:t) = foldList fun (fun h acc) t


elemList :: Eq a => a -> [a] -> Bool
elemList el list = foldList (\e ac -> (ac || (e == el))) False list


appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldList (\e ac -> (e:ac)) l2 (reverse l1) 


listLength :: [a] -> Integer
listLength list = foldList (\e ac -> ac + 1) 0 list


filterList :: (a -> Bool) -> [a] -> [a]
filterList pre list = 
  foldList (\e a -> if pre e then (e:a) else a) [] (reverse list)


nth :: [a] -> Integer -> a
nth list@(h:t) n = if len <= n then error "List too short." else el
  where 
    (len, el) = foldList (\e (k, a) -> (k+1, if k==n then e else a)) (0, h) list 


mapList :: (a -> b) -> [a] -> [b]
mapList fun list = foldList (\e a -> (fun e):a) [] list


andList :: [Bool] -> Bool
andList list = foldList (\e a -> e && a) True list


allList :: (a-> Bool) -> [a] -> Bool
allList pre list = foldList (\e a -> pre e && a) True list


isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = 
  dfs initial (\x -> if x == initial then True else False) True []
    where
      dfs _ _ False _ = False
      dfs v scheduled acc queue =
        case queue' of (h:t) -> dfs h scheduled' acc' t
                       []    -> acc'

        where
          acc' = acc && (isOk v)
          queue' = appendList queue tovisit
          tovisit = (filterList (\x -> not (scheduled x)) (neighbours v))
          scheduled' = enrich scheduled tovisit


reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = isReachable
  where
    (isReachable, _) = aux initial (\x -> False)
    aux initial' visited = 
      foldList (\e ac -> f e ac) (False, visited) (neighbours initial')
      where
        f v' (isReachable, visited') = 
          if visited' v' then (isReachable, visited')
          else if v == v' then (True, enrich visited' [v'])
          else aux v' (enrich visited' [v'])


allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = 
  allList (\x -> reachable x initial neighbours) vs


isClosed :: Maze -> Bool
isClosed (Maze initial m) = goodStart 
  && isGraphClosed initial neighbours (\x -> not (Blank == m x))
  where
    neighbours (C x y) = filterList (\z -> not(Wall == m z))
      [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
    goodStart = m initial == Storage || m initial == Ground


isSane :: Maze -> Bool
isSane (Maze initial m) = reachableBoxes <= reachableStorages
  where
    (reachableBoxes, _) = reachableTiles initial Box (\x -> False) 0
    (reachableStorages, _) = reachableTiles initial Storage (\x -> False) 0
    neighbours c@(C x y) = if m c == Wall || m c == Blank then [] 
      else [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
    reachableTiles v tile visited count =
      foldList (\e ac -> f e ac) (count, visited) (neighbours v)
      where
        f v' (count', visited') =
          if visited' v' then (count', visited')
          else
            if m v' == tile then ((nc + 1), nv)
            else (nc, nv)
            where
              (nc, nv) = reachableTiles v' tile (enrich visited' [v']) count'


enrich :: Eq a => (a -> Bool) -> [a] -> (a -> Bool)
enrich fun [] = fun
enrich fun (h:t) = enrich fun' t
  where
    fun' x = if x == h then True else fun x        


pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

