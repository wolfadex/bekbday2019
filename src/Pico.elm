module Pico exposing (Tile(..), Position, Board, createBoard, toggleTile, xyToIndex)


import Array exposing (Array)


type Tile
    = Solid
    | Empty
    | Clue (List Int)
    | Spacer


type alias Position =
    ( Int, Int )


type alias Board =
    { current : Array Tile
    , answer : Array Tile
    , width : Int
    , height : Int
    , isComplete : Bool
    }


createBoard : Array Tile -> Int -> Int -> Board
createBoard answer width height =
    { current = Array.map
          (\t ->
              case t of
                  Solid -> Empty
                  _ -> t
          )
          answer
    , answer = answer
    , width = width
    , height = height
    , isComplete = False
    }


checkBoard : Board -> Board
checkBoard ({ current, answer } as board) =
    let
        left = Array.toList current
        right = Array.toList answer
    in
        { board
        | isComplete =
              List.length left == List.length right
              && List.all identity (List.map2 (\l r -> l == r) left right)
        }


toggleTile : Board -> Position -> Board
toggleTile ({ current, width } as board) pos =
    let
        index = xyToIndex pos width
        nextTile = case Array.get index current of
            Just Empty ->
                Just Solid
            Just Solid ->
                Just Empty
            _ ->
                Nothing
    in
        case nextTile of
            Nothing ->
                board
            Just tile ->
                checkBoard { board | current = Array.set index tile current }
                


indexToXY : Int -> Int -> Position
indexToXY index width =
    ( modBy index width, index // width )


xyToIndex : Position -> Int -> Int
xyToIndex ( x, y ) width =
    x + width * y