import gameboard exposing (board)
import List exposing (length, repeat, map, indexedMap, head, drop, foldl, sum)
import Graphics.Element exposing (show, Element, flow, down, right)
import Time exposing (every, second, fps)
import Window exposing (dimensions)
import Graphics.Collage exposing (collage, filled, rect)
import Color exposing (..)
import Maybe

-- Model
type alias GameState = List (List Bool)

-- Update
step : Float -> GameState -> GameState
step t gameState =
  let
    -- Map the board indexes to each position
    gameStateIndexes : List (List ((Int,Int), Bool))
    gameStateIndexes = List.map (\(y,l) -> (List.map (\(x,e) -> ((x,y), e)) l)) (List.indexedMap (,) (List.map (List.indexedMap (,)) gameState)) -- [[((0,0),False)],[((0,1), False)]]
    -- Create a function to tell whether a given cell will live
    cellLives : List (List ((Int,Int), Bool)) -> (Int,Int) -> Bool
    cellLives boardList (x,y) =
      let 
        -- This function takes boardList and and (x,y) and returns the index of the list at (x,y)
        getAtXY l (x,y) = (head (drop y l)) `Maybe.andThen` (\a -> (head (drop x a))) |> Maybe.withDefault ((0,0),False)
        -- Create a list of neigbours relative to a position
        neighbours = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]
        -- Map the list of neigbours with whether or not that position is on
        neighboursAlive = (map (getAtXY boardList >> snd) neighbours)
        -- Convert the list from Bool to Int
        neighboursAliveInt = map (\a -> if a then 1 else 0) neighboursAlive
        -- Get the sum of the list
        numAliveNeighbours = sum neighboursAliveInt
        -- Get whether the current cell is alive
        curCellAlive = snd (getAtXY boardList (x,y))
        -- This function returns whether a cell lives based on its alive neighbours and whether it is already alive
        cellRules : Int -> Bool -> Bool
        cellRules aliveNeighbours isAlive =
          if isAlive
          then 
              if
              | aliveNeighbours < 2 -> False
              | aliveNeighbours > 3 -> False
              | otherwise -> True -- == 2 || 3
          else
              aliveNeighbours == 3
     in
       -- Return whether the cell will die based on the list sum, and if it already alive
       cellRules numAliveNeighbours curCellAlive
   in
     map (\l -> (map (\((x,y),a) -> (cellLives gameStateIndexes (x,y))) l)) gameStateIndexes
    
-- View
view : (Int, Int) -> GameState -> Element
view (w,h) gameState =
  let 
    drawBlock w b list =
      let 
        c = if b then white else black
        size = (w//(length list))
        sizeFloat  = toFloat size 
      in
        collage size size [filled c (rect sizeFloat sizeFloat)]
    mappedElements = map (\l -> (map (\e -> (drawBlock w e gameState)) l)) gameState
  in
    flow down (map (flow right) mappedElements)


-- Putting it all together (Signal)
gameState : Signal GameState
gameState =
    Signal.foldp step board (fps 5)


main : Signal Element
main =
    Signal.map2 view Window.dimensions gameState