import GameBoard exposing (board)
import List exposing (length, repeat, map, indexedMap, head, drop, foldl, sum)
import Graphics.Element exposing (show, Element, flow, down, right)
import Time exposing (every, second, fps)
import Window exposing (dimensions)
import Graphics.Collage exposing (collage, filled, rect)
import Color exposing (..)
import Maybe

-- Model
type alias GameState = List (List Bool)
type alias Point = (Int, Int)
type alias Cell = (Point, Bool)
type alias Board = List (List Cell)

-- Update
step : Float -> GameState -> GameState
step t gameState =
  let
    -- Map the board indexes to each position
    gameStateIndexes : Board
    gameStateIndexes = List.map (\(y,l) -> (List.map (\(x,e) -> ((x,y), e)) l)) (List.indexedMap (,) (List.map (List.indexedMap (,)) gameState)) -- [[((0,0),False)],[((0,1), False)]]
    -- Create a function to tell whether a given cell will live
    cellLives : Board -> Point -> Bool
    cellLives boardList (x,y) =
      let 
        getAtXY l (x,y) = (head (drop y l)) `Maybe.andThen` (\a -> (head (drop x a))) |> Maybe.withDefault ((0,0),False)
        neighbours = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]
        numAliveNeighbours = sum <| map (\a -> if a then 1 else 0) (map (getAtXY boardList >> snd) neighbours)
        curCellAlive = snd (getAtXY boardList (x,y))
        -- This function returns whether a cell lives based on its alive neighbours and whether it is already alive
        willCellLive : Int -> Bool -> Bool
        willCellLive aliveNeighbours isAlive =
          if isAlive
          then 
              if
              | aliveNeighbours < 2 -> False
              | aliveNeighbours > 3 -> False
              | otherwise -> True -- == 2 || 3
          else
              aliveNeighbours == 3
     in
       -- Return whether the cell will live based on the list sum, and if it already alive
       willCellLive numAliveNeighbours curCellAlive
   in
     map (\l -> (map (\((x,y),a) -> (cellLives gameStateIndexes (x,y))) l)) gameStateIndexes
    
-- View
view : Point -> GameState -> Element
view (w,h) gameState =
  let 
    listSize = length gameState
    drawBlock w b listSize =
      let 
        c = if b then white else black
        size = (w//listSize)
        sizeFloat  = toFloat size 
      in
        collage size size [filled c (rect sizeFloat sizeFloat)]
    mappedElements = map (\l -> (map (\e -> (drawBlock w e listSize)) l)) gameState
  in
    flow down (map (flow right) mappedElements)


-- Putting it all together (Signal)
gameState : Signal GameState
gameState =
    Signal.foldp step board (fps 5)


main : Signal Element
main =
    Signal.map2 view Window.dimensions gameState
