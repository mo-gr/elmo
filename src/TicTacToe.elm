module TicTacToe exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type alias Model =
    { field : Dict ( Int, Int ) Player
    , nextPlayer : Player
    }


type Player
    = X
    | O


type Row
    = R1
    | R2
    | R3


rowNum : Row -> Int
rowNum r =
    case r of
        R1 ->
            1

        R2 ->
            2

        R3 ->
            3


type Col
    = C1
    | C2
    | C3


colNum : Col -> Int
colNum r =
    case r of
        C1 ->
            1

        C2 ->
            2

        C3 ->
            3


toKey : Row -> Col -> ( Int, Int )
toKey row col =
    ( rowNum row, colNum col )


type Msg
    = Move Row Col
    | Reset
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map toKeyMsg (Decode.field "key" Decode.string))


toKeyMsg : String -> Msg
toKeyMsg string =
    case string of
        "r" ->
            Reset

        _ ->
            NoOp


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { field = Dict.empty, nextPlayer = X }, Cmd.none )


allSame : Dict ( Int, Int ) Player -> List ( Int, Int ) -> Bool
allSame field coords =
    List.all (\c -> Dict.get c field == Just X) coords
        || List.all (\c -> Dict.get c field == Just O) coords


hasWinner : Model -> Bool
hasWinner { field } =
    allSame field [ toKey R1 C1, toKey R2 C2, toKey R3 C3 ]
        || allSame field [ toKey R3 C3, toKey R2 C2, toKey R1 C1 ]
        || allSame field [ toKey R1 C3, toKey R2 C2, toKey R3 C1 ]
        || allSame field [ toKey R3 C1, toKey R2 C2, toKey R1 C3 ]
        || List.any (\r -> allSame field [ toKey r C1, toKey r C2, toKey r C3 ]) rows
        || List.any (\c -> allSame field [ toKey R1 c, toKey R2 c, toKey R3 c ]) cols


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { field = Dict.empty, nextPlayer = X }, Cmd.none )

        Move r c ->
            if hasWinner model then
                ( model, Cmd.none )

            else
                case Dict.get (toKey r c) model.field of
                    Just _ ->
                        ( model, Cmd.none )

                    Nothing ->
                        ( { model
                            | field = Dict.insert (toKey r c) model.nextPlayer model.field
                            , nextPlayer =
                                case model.nextPlayer of
                                    X ->
                                        O

                                    O ->
                                        X
                          }
                        , Cmd.none
                        )


rows : List Row
rows =
    [ R1, R2, R3 ]


cols : List Col
cols =
    [ C1, C2, C3 ]


block : Model -> Row -> Col -> Html.Html Msg
block model row col =
    div
        [ onClick (Move row col)
        , style "backgroundColor" "white"
        , style "width" "90px"
        , style "height" "90px"
        , style "lineHeight" "90px"
        , style "textAlign" "center"
        , style "fontSize" "90px"
        , style "position" "fixed"
        , style "left"
            (case col of
                C1 ->
                    "0"

                C2 ->
                    "105px"

                C3 ->
                    "210px"
            )
        , style "top"
            (case row of
                R1 ->
                    "0"

                R2 ->
                    "105px"

                R3 ->
                    "210px"
            )
        ]
        [ text
            (case Dict.get (toKey row col) model.field of
                Just X ->
                    "X"

                Just O ->
                    "O"

                Nothing ->
                    ""
            )
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div
            [ style "backgroundColor"
                (if hasWinner model then
                    "red"

                 else
                    "black"
                )
            , style "width" "300px"
            , style "height" "300px"
            , style "position" "relative"
            ]
            (List.concatMap (\r -> List.map (\c -> block model r c) cols) rows)
        , div
            [ style "textAlign" "center"
            , style "width" "300px"
            ]
            [ text
                (case ( hasWinner model, model.nextPlayer ) of
                    ( True, X ) ->
                        "Winner is O! Press 'R' to reset."

                    ( True, O ) ->
                        "Winner is X! Press 'R' to reset."

                    ( False, X ) ->
                        "Next move: X"

                    ( False, O ) ->
                        "Next move: O"
                )
            ]
        ]
