import Html exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (..)

main : Program Never Model Msg
main = 
    program
        {
            init = init
        ,   view = view
        ,   update = update
        ,   subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  mkTable model.arr

init : (Model, Cmd msg)
init = ({arr = (square 3 (\_ -> Nothing)), turn = X}, Cmd.none)

type Player = X | O

type alias Model = 
    {
        arr : Matrix (Maybe Player)
    ,   turn : Player
    }

nextTurn : Player -> Player
nextTurn turn = 
    case turn of
        X -> O
        O -> X

type Msg = Play Int Int

viewPlayer : Player -> String
viewPlayer p =
    case p of
        X -> "X"
        O -> "O"

viewElement : Maybe (Maybe Player) -> Html msg
viewElement element =
    let
        el = joinMaybe element
    in
        case el of
            Nothing -> text ""
            Just p  -> text (viewPlayer p)


mkTable : Matrix (Maybe Player) -> Html Msg
mkTable m =
    let
        element i j = td [ onClick (Play i j) ] [ viewElement (get (i, j) m) ]
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

