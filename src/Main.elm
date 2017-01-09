module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Array as A
import Debug as D
import Native.MyLazy

memoize : (a -> b) -> (a -> b)
memoize = Native.MyLazy.memoize


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

type Player = Human | Computer

type alias Board = Matrix (Maybe Player)

type alias Model = 
    {
        arr : Board
    ,   turn : Player
    }

init : (Model, Cmd msg)
init = ({arr = emptyArray, turn = Human}, Cmd.none)

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
        Human -> "X"
        Computer -> "O"

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


makeMove : Player -> Location -> Board -> Board
makeMove player (i, j) board = set (i, j) (Just player) board


play : Int -> Int -> Msg -> Model -> (Model, Cmd Msg)
play i j msg model =
    let
        array = model.arr
        currturn = model.turn
        element = joinMaybe <| get (i, j) array
        gameOver = playerOutcome array currturn |> isJust 
    in
        if gameOver
            then (model, Cmd.none)
            else
                case element of
                    Nothing -> computerPlay { model | arr = makeMove currturn (i, j) array, turn = otherPlayer currturn}
                    _       -> (model , Cmd.none)
        

computerPlay : Model -> (Model, Cmd Msg)
computerPlay model =
    let
        computer = model.turn
        nextArray = minimax2 computer model.arr
        player = otherPlayer computer
    in
        ({ model | arr = nextArray, turn = player}, Cmd.none)
        

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Play i j -> play i j msg model
    Reset    -> init


otherPlayer : Player -> Player
otherPlayer turn = 
    case turn of
        Human -> Computer
        Computer -> Human


-- Computer player


isNothing : Maybe a -> Bool
isNothing m = case m of
    Nothing -> True
    _ -> False

isJust : Maybe a -> Bool
isJust = not << isNothing


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
        antiD = case insideOut <| List.map2 (\n row -> List.head << List.drop n <| row) (List.reverse <| List.range 0 2) tboard of
            Just d -> d
            Nothing -> []
    in
      board ++ tboard ++ [mainD] ++ [antiD]

winner : Board -> Maybe Player
winner b =
    let
        ts = triples b
    in
        if List.member (List.repeat 3 (Just Human)) ts then Just Human
        else if List.member (List.repeat 3 (Just Computer)) ts then Just Computer
        else Nothing


playerOutcome : Board -> Player -> Maybe Outcome
playerOutcome b player = 
    let
        w = winner b
        flatBoard = flatten b
    in
        case w of
            Just p -> if p == player then Just Win else Just Lose
            Nothing -> if List.all (not << isNothing) flatBoard then Just Draw else Nothing

mySnd : (a, b, c) -> b
mySnd (_, b, _) = b


rank : Outcome -> Int
rank o =
    case o of
        Win -> 10
        Lose -> -10
        Draw -> 0


filterMaybe : List (Maybe a) -> List a
filterMaybe lma = 
    case lma of
        [] -> []
        (mx::mxs) -> 
            case mx of
                Just x -> x :: filterMaybe mxs
                Nothing -> filterMaybe mxs


filterJustTuple : List (Maybe a, b) -> List (a, b)
filterJustTuple lma = 
    case lma of
        [] -> []
        ((mx, y)::mxsys) -> 
            case mx of
                Just x -> (x, y) :: filterJustTuple mxsys
                Nothing -> filterJustTuple mxsys

filterNothingTuple : List (Maybe a, b) -> List (Maybe a, b)
filterNothingTuple = List.filter (isNothing << Tuple.first)


reverseOutcome : Outcome -> Outcome
reverseOutcome o = case o of
    Win -> Lose
    Lose -> Win
    Draw -> Draw


rank2 : Outcome -> Int -> Int
rank2 out depth = case out of
    Win -> 10 - depth
    Lose -> -10 + depth
    Draw -> 0


minimax2 : Player -> Board -> Board
minimax2 player board =
    let
        helper_ : (Player, Int, Board) -> (Outcome, Int)
        helper_ (pp, dd, bb) =
            case playerOutcome bb pp of
                Just o -> (o, dd)
                Nothing -> 
                    let
                        moves = availableMoves bb
                        other = otherPlayer pp
                        bs = List.map (\pos -> makeMove other pos bb) moves
                        outs = List.map (\b -> helper (other, (dd + 1), b)) bs
                        ranks = outs |> List.map (\(o, d) -> ((o, d), rank2 o d)) |> List.sortBy Tuple.second |> List.reverse
                    in
                        case List.head ranks of
                            Just ((o, d), r) -> (reverseOutcome o, d)
                            Nothing -> D.crash "Boom!"
        helper = memoize helper_ 
        gameOver = playerOutcome board player |> isJust
        allBoards = availableMoves board |> List.map (\pos -> makeMove player pos board)
        tagged = allBoards |> List.map (\b -> (b, helper (player, 0, b) |> (\(o, d) -> rank2 o d))) |> List.sortBy Tuple.second |> List.reverse
    in
        if gameOver
            then board
            else
                case List.head tagged of
                    Nothing -> D.crash "Argh!"
                    Just (b, r) -> b
