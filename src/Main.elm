module Main exposing (..)

import Browser
import Html exposing (div, text, button)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (..)

import List.Extra exposing (setAt, getAt, updateAt, transpose, splitAt)
import Maybe.Extra exposing (values)

main = Browser.element { init = init, update = update, subscriptions = subs, view = view}

-- Invariants to mantain manually:
--   . The shape of the current board is always 12x4
--   . A move with 0 slides should never be in the previous moves list
--     otherwise the movecount will be wrong
type alias Model = { currentPuzzle : Puzzle
                   , previousMoves : List Move
                   , currentBoard : Board
                   , cursorMode : CursorMode
                   , moveInProgress : Move 

                   -- The row or col the cursor is in
                   -- always interpreted modulo length of the list
                   , cursorIndex : Int
                   }

type Msg = Slide Dir 
         | MoveCursor Dir 
         | ChangeCursorMode 
         | ResetPuzzle 
         | Undo
         | NoOp

type Dir = Up | Down
type CursorMode = MovingRow | MovingColumn

-- The index of the row/col and how many tiles to slide it over (possibly negative)
type Move = RowMove Int Int | ColMove Int Int
type alias Puzzle = { startingPosition : Board
                    , maximumMoveCount : Int
                    }
type alias Board = List (List Bool)

t = True
f = False
-- 12 x 4
checkeredPattern = { startingPosition = [ [f, t, f, t, f, t, f, t, f, t, f, t]
                                        , [t, f, t, f, t, f, t, f, t, f, t, f]
                                        , [f, t, f, t, f, t, f, t, f, t, f, t]
                                        , [t, f, t, f, t, f, t, f, t, f, t, f]
                                        ]
                   -- Infinite moves for the sake of easier testing
                   , maximumMoveCount = -1
                   }

puzzle1 = { startingPosition = [ [t, f, t, f, f, t, f, f, f, f, f, f]
                               , [f, t, f, f, t, f, f, f, f, f, f, t]
                               , [f, t, f, f, f, f, f, f, f, f, f, f]
                               , [f, t, f, f, f, f, f, f, f, f, f, f]
                               ]
          , maximumMoveCount = 3
          }

init : () -> (Model, Cmd Msg)
init _ = (initPuzzle puzzle1, Cmd.none)

initPuzzle : Puzzle -> Model
initPuzzle p =
  { currentBoard = p.startingPosition
  , previousMoves = []
  , currentPuzzle = p
  , cursorMode = MovingColumn 
  , moveInProgress = ColMove 0 0
  , cursorIndex = 0 
  }

keyDecoder : Decode.Decoder Msg
keyDecoder = Decode.map keyToMessage <| Decode.field "key" Decode.string

keyToMessage : String -> Msg
keyToMessage key =
  case key of
    "K" -> Slide Up
    "k" -> MoveCursor Up
    "J" -> Slide Down
    "j" -> MoveCursor Down
    "r" -> ResetPuzzle
    "u" -> Undo
    " " -> ChangeCursorMode
    _ -> Debug.log key NoOp

subs _ = onKeyDown keyDecoder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  withNoCmd <|
    case msg of
      Slide dir ->  Debug.log "model:" <| slideModel model dir
      MoveCursor dir ->
        { model | cursorIndex = model.cursorIndex + dirAsOffset dir}
      ChangeCursorMode ->
        { model |
          cursorMode = changeMode model.cursorMode
        -- Always reset the position to the starting one
        , cursorIndex = 0}
      ResetPuzzle ->
        -- Keep the position of the cursor
        let resetModel = initPuzzle model.currentPuzzle
        in { resetModel | cursorIndex = model.cursorIndex, cursorMode = model.cursorMode}
      Undo ->
        if isZeroDistanceMove model.moveInProgress then
          case model.previousMoves of
            -- No moves to undo
            [] -> model
            (x::xs) -> { model | previousMoves = xs, currentBoard = applyInverse x model.currentBoard }
        else
          { model | moveInProgress = ColMove 0 0, currentBoard = applyInverse model.moveInProgress model.currentBoard }
      NoOp -> model 


withNoCmd x = (x, Cmd.none)

slideModel : Model -> Dir -> Model
slideModel model dir =
  case moveAsContinuation model.moveInProgress model.cursorMode model.cursorIndex dir of
    Nothing -> 
      if List.length model.previousMoves + 1 >= model.currentPuzzle.maximumMoveCount then
        -- If the player has used up all his moves then not allow moves that arent continuations
        -- of the previous move. (Continuations are fine)
        model 
      else
        -- Prevent zero length moves from getting in the move history
        (if Debug.log "zdm: " (isZeroDistanceMove model.moveInProgress) then model
        else { model | previousMoves = model.moveInProgress :: model.previousMoves })
          |> updateMoveInProgress dir
          |> slideBoard dir (cursorModeToSlidingFunction model.cursorMode)

    Just move -> { model | moveInProgress = move }
                   |> slideBoard dir (cursorModeToSlidingFunction model.cursorMode)

applyMove : Move -> Board -> Board
applyMove m = case m of
                RowMove i c -> shiftRow i c
                ColMove i c -> shiftColumn i c

applyInverse = applyMove << inverseMove

updateMoveInProgress : Dir -> Model -> Model
updateMoveInProgress dir m = { m | moveInProgress = cursorModeAsMoveConstructor m.cursorMode m.cursorIndex (dirAsOffset dir) }

cursorModeAsMoveConstructor : CursorMode -> Int -> Int -> Move
cursorModeAsMoveConstructor mode = case mode of
                                     MovingColumn -> ColMove
                                     MovingRow -> RowMove

cursorModeToSlidingFunction : CursorMode -> Int -> Dir -> List (List a) -> List (List a)
cursorModeToSlidingFunction mode = case mode of
                                     MovingColumn -> slideColumn
                                     MovingRow -> slideRow

isZeroDistanceMove : Move -> Bool
isZeroDistanceMove move = case move of
                            RowMove _ i -> i == 0
                            ColMove _ i -> i == 0

inverseMove : Move -> Move
inverseMove m = case m of
                  RowMove i c -> RowMove i (-c)
                  ColMove i c -> ColMove i (-c)

moveAsContinuation : Move -> CursorMode -> Int -> Dir -> Maybe Move
moveAsContinuation originalMove contMode contIndex contDirection =
  let possibleContinuation =
        \mod m i c -> if modBy mod i == modBy mod contIndex then
                        Just <| m i (c+dirAsOffset contDirection)
                      else Nothing 

  in case originalMove of
    RowMove i c ->
      case contMode of
        MovingRow -> possibleContinuation 4 RowMove i c
        MovingColumn -> Nothing
    ColMove i c ->
      case contMode of
        MovingRow -> Nothing
        MovingColumn -> possibleContinuation 6 ColMove i c
 
slideBoard dir slidingFunction model = { model |
                                         currentBoard = slidingFunction model.cursorIndex dir model.currentBoard
                                       }

-- Consider making a updateModuloLength function
shiftRow : Int -> Int -> List (List a) -> List (List a)
shiftRow i shiftCount = updateAt (modBy 4 i) (shift <| shiftCount)

slideRow : Int -> Dir -> List (List a) -> List (List a)
slideRow i dir = shiftRow i (dirAsOffset dir)

shiftColumn : Int -> Int -> List (List a) -> List (List a)
shiftColumn colIndex shiftCount rows =
  let cols = transpose rows
      selectedCol = getModuloLength colIndex cols
      -- Depends on the length of the board
      -- generically it's (colIndex + length/2) mod length
      diametricallyOpposedCol = getModuloLength (colIndex + 6) cols
      -- Reverse one so the part where the cols touch is the the middle
      circleSlice = List.reverse selectedCol ++ diametricallyOpposedCol
      shiftedSlice = shift shiftCount circleSlice
      (firstSlice, secondSlice) = splitAt 4 shiftedSlice
  in cols
      -- Not forget to reverse the first slice back
      |> setModuloLength colIndex (List.reverse firstSlice)
      |> setModuloLength (colIndex + 6) secondSlice
      |> transpose 

slideColumn : Int -> Dir -> List (List a) -> List (List a)
slideColumn colIndex dir = shiftColumn colIndex (dirAsOffset dir)

-- Will crash if the index is zero
-- will hang forever if I am a bad coder
getModuloLength : Int -> List a -> a
getModuloLength i l =
  let guaranteedlyInRangeIndex = modBy (List.length l) i
  in case getAt guaranteedlyInRangeIndex l of
    -- Should never happen, so let's recurse forever to bypass the typechecker
    Nothing -> getModuloLength i l
    Just val -> val

-- Will crash if the index is zero
setModuloLength : Int -> a -> List a -> List a
setModuloLength i value l = setAt (modBy (List.length l) i) value l

shift : Int -> List a -> List a
shift offset l = List.indexedMap
                   -- (\i _ -> getAt (modBy (List.length l) (i + offset)) l) l
                   (\i _ -> getModuloLength (i + offset) l) l

dirAsOffset : Dir -> Int
dirAsOffset dir = case dir of
                    Up -> 1
                    Down -> -1

changeMode : CursorMode -> CursorMode
changeMode mode = case mode of
                    MovingColumn -> MovingRow
                    MovingRow -> MovingColumn

-- View stuff

-- TODO: rename circleStuff to boardStuff
circleHeight = 500
circleWidth = 500
circleRadius = 200
circleCenterY = circleHeight / 2
circleCenterX = circleWidth / 2

drawTile : Bool -> Int -> Int -> Bool -> Svg Msg
drawTile hasEnemy indexX indexY isSelected = 
  -- x is like the angle and y is like the distance from the origin
  let anglePerTile = 2 * pi / 12
      halfTileAngleOffset = anglePerTile / 2
      -- The offset makes the angle correspond to the middle of the tile
      tileAngle = toFloat indexX * anglePerTile + halfTileAngleOffset
      distancePerTile = circleRadius / 4
      distanceOffset = distancePerTile / 2
      tileDistanceFromTheCenter = toFloat indexY * distancePerTile + distanceOffset
      posX = round <| cos tileAngle * tileDistanceFromTheCenter + circleCenterX
      posY = round <| sin tileAngle * tileDistanceFromTheCenter + circleCenterY
  in circle ([ cx (String.fromInt posX)
            , cy (String.fromInt posY)
            -- TODO: Make the circles closer to the center smaller
            , r (String.fromInt <| round <| tileDistanceFromTheCenter/10 + 4)
            , fill (tileColor hasEnemy)
            ]
              ++ (if isSelected then
            [ stroke "black"
            , strokeWidth "3"
            ] else [])) []

tileColor : Bool -> String
tileColor hasEnemy = case hasEnemy of
                      True -> "red"
                      False -> "blue"

isTileSelected : Int -> Int -> CursorMode -> Int -> Bool
isTileSelected tileX tileY cursorMode cursorPos =
  case cursorMode of
    MovingColumn -> modBy 6 tileX == modBy 6 cursorPos
    MovingRow -> modBy 4 tileY == modBy 4 cursorPos

view : Model -> Html.Html Msg
-- view model = text <| Debug.toString model
view model =
  Html.div []
  [svg
    [ width (String.fromInt circleHeight)
    , height (String.fromInt circleWidth)
    , viewBox <| "0 0 " ++ (String.fromInt circleHeight) ++ " " ++ (String.fromInt circleWidth)
    ]
    (model.currentBoard
      |> List.indexedMap (\y -> List.indexedMap (\x tile -> drawTile tile x y (isTileSelected x y model.cursorMode model.cursorIndex)))
      |> List.concat
    )
    , text <| (String.fromInt <| List.length model.previousMoves + 1) ++ "/" ++ (String.fromInt model.currentPuzzle.maximumMoveCount)
  ]
