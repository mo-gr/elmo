module BasicCounter exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = Increment


main =
    Browser.sandbox
        { init = 0
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update Increment n =
    n + 1


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        ]
        [ div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
