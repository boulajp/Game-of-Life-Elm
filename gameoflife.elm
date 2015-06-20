--import gameboard exposing (board)
import List exposing (length, repeat, map)
import Graphics.Element exposing (show, Element)
import Time exposing (every, second)
import Window exposing (dimensions)

-- Model
type alias GameState = List (List Bool)

board : GameState
board = 
    [
        [0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
       ,[0,0,0,0,0,0,0,0,0]
    ]

-- Update
step : Float -> GameState -> GameState
step t gameState =
  let
   -- 1. Create a new 2-dimensional list with a 0 border (List will now be of dimension (n+1)x(n+1))
        -- This represents the top and bottom lines
        additionalLine : List Bool
        additionalLine = repeat ((length gameState) + 1) 0
        -- This represents the middle lines
        middleLines : List (List Bool)
        middleLines = map (\l -> 0 :: (l ++ [0])) gameState
        -- Concatenate the lists
        gameStateWithBorder = additionalLine :: (middleLines ++ [additionalLine])
   -- 2. Map some recursive foldl take3 + to the list
   gameStateNfoldl (+) 0 (foldl (+) 0 <| take 3 gameStateWithBorder)
   -- 3. Map the counting function to the list
  in
    gameStateWithBorder
    

-- View
view : (Int, Int) -> GameState -> Element
view (w,h) gameState =
   -- flow down (map flow right gameState)
   show gameState


-- Putting it all together (Signal)
gameState : Signal GameState
gameState =
    Signal.foldp step board (every second)


main : Signal Element
main =
    Signal.map2 view Window.dimensions gameState