module BasicGet exposing (..)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, index, string)


type Model
    = Nothing
    | Loading
    | Loaded String


type Msg
    = GotResponse (Result Http.Error String)
    | Fetch


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = noSubscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, Cmd.none )


noSubscriptions : Model -> Sub Msg
noSubscriptions _ =
    Sub.none


catDecoder : Decoder String
catDecoder =
    index 0 <| field "url" string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Fetch ->
            ( Loading
            , Http.get
                { url = "https://api.thecatapi.com/v1/images/search"
                , expect = Http.expectJson GotResponse catDecoder
                }
            )

        GotResponse (Ok value) ->
            ( Loaded value, Cmd.none )

        GotResponse (Err _) ->
            ( Nothing, Cmd.none )


mainStyle : List (Html.Attribute msg)
mainStyle =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "text-align" " center"
    , style "flex-direction" " column"
    , style "max-width" "80vw"
    , style "margin" "auto"
    ]


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            div mainStyle
                [ div [] [ text "nothing yet" ]
                , button [ onClick Fetch ] [ text "Fetch a cat" ]
                ]

        Loading ->
            div mainStyle
                [ div [] [ text "loading..." ]
                ]

        Loaded resp ->
            div mainStyle
                [ img [ src resp ] []
                , button [ onClick Fetch ] [ text "Fetch another cat" ]
                ]
