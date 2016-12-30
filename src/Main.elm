module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Array as A


main : Program Never Model Msg
main = 
    program
        {
            init = init
        ,   view = view
        ,   update = update
        ,   subscriptions = subscriptions
        }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Model

type Player = X | O

type alias Board = Matrix (Maybe Player)

type alias Model = 
    {
        arr : Board
    ,   turn : Player
    }

init : (Model, Cmd msg)
init = ({arr = emptyArray, turn = X}, Cmd.none)

emptyArray : Matrix (Maybe Player)
emptyArray = square 3 (\_ -> Nothing)

-- View

view : Model -> Html Msg
view model =
    div []
        [
            mkTable model.arr
        ,   br [] []
        ,   button [ onClick Reset ] [ text "Reset" ]
        ]


viewPlayer : Player -> String
viewPlayer p =
    case p of
        X -> "X"
        O -> "O"

renderCell : Maybe (Maybe Player) -> Html msg
renderCell element =
    let
        el = joinMaybe element
    in
        case el of
            Nothing -> text ""
            Just p  -> text (viewPlayer p)


mkTable : Matrix (Maybe Player) -> Html Msg
mkTable m =
    let
        element i j = td [ onClick (Play i j) ] [ renderCell (get (i, j) m) ]
        line : Int -> List (Html Msg)
        line i = 
            [
                tr 
                    []
                    <| List.foldl (\x acc -> [element i x] ++ acc) [] (List.range 0 2)
            ]
    in
        table
            []
            [
                tbody [] (List.concatMap line (List.range 0 2))
            ]

joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe mma =
    case mma of
        Nothing -> Nothing
        Just Nothing -> Nothing
        Just (Just a) -> Just a

-- Update

type Msg = Play Int Int
         | Reset

play : Int -> Int -> Msg -> Model -> (Model, Cmd Msg)
play i j msg model =
    let
        array = model.arr
        currturn = model.turn
        element = joinMaybe <| get (i, j) array
    in
        case element of
            Nothing -> ({ model | arr = set (i, j) (Just currturn) array, turn = nextTurn currturn}, Cmd.none)
            _       -> (model , Cmd.none)
        

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Play i j -> play i j msg model
    Reset    -> init


nextTurn : Player -> Player
nextTurn turn = 
    case turn of
        X -> O
        O -> X


-- Computer player


isNothing : Maybe a -> Bool
isNothing m = case m of
    Nothing -> True
    _ -> False


availableMoves : Matrix (Maybe Player) -> List Location
availableMoves m =
      List.map Tuple.first <| List.filter (isNothing << Tuple.second) <| flatten <| Matrix.mapWithLocation (,) m


type Outcome = Lose | Draw | Win

-- http://stackoverflow.com/questions/31932683/transpose-in-elm-without-maybe

mCons : a -> Maybe (List a) -> Maybe (List a)
mCons v ml = Maybe.map ((::) v) ml

(#^) v ml = mCons v ml

m2Cons : Maybe a -> Maybe (List a) -> Maybe (List a)
m2Cons ma mlb = Maybe.map2 (::) ma mlb

(^#^) mla mlb = m2Cons mla mlb

insideOut : List (Maybe a) -> Maybe (List a)
insideOut lm = case lm of
    [] -> Just []
    (Just x) :: mxs -> x #^ insideOut mxs
    Nothing :: _ -> Nothing 

transpose : List (List a) -> Maybe (List (List a))
transpose ll = case ll of
    ((x :: xs) :: xxs) -> -- We have at least one non-empty list at head
        let
          mheads = 
            xxs
            |> List.map List.head 
            |> insideOut
          mtails = 
            xxs
            |> List.map List.tail
            |> insideOut
        in
          (x #^ mheads) ^#^ (Maybe.andThen transpose (xs #^ mtails))
    _ ->
        -- The head list is empty. Only return something if all others are empty as well.
        if ll == List.filter List.isEmpty ll then
            Just []
        else
            Nothing

triples : Board -> List (List (Maybe Player))
triples b = 
    let
        board =
            b
            |> A.map A.toList
            |> A.toList
        -- This shouldn't fail, by construction...
        tboard = case transpose board of
            Just t -> t
            Nothing -> []
        mainD = case insideOut <| List.map2 (\n row -> List.head << List.drop n <| row) (List.range 0 2) board of
            Just d -> d
            Nothing -> []
        antiD = case insideOut <| List.map2 (\n row -> List.head << List.drop n <| row) (List.range 0 2) tboard of
            Just d -> d
            Nothing -> []
    in
      board ++ tboard ++ [mainD] ++ [antiD]


      