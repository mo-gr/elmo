module Brownbag exposing (..)

import Browser
import Html exposing (div, input, text)
import Html.Attributes exposing (attribute, checked)
import Html.Events exposing (onClick)
import Json.Encode exposing (int)


type Msg
    = ToggleTodo TodoId


type TodoState
    = Open
    | Done


type TodoId
    = TodoId Int


type alias Todo =
    { id : TodoId
    , name : String
    , state : TodoState
    }


type alias Model =
    { todos : List Todo
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos =
            [ { name = "Learn Elm", state = Open, id = TodoId 1 }
            , { name = "Meet in Zoom", state = Done, id = TodoId 2 }
            ]
      }
    , Cmd.none
    )


toggleState : TodoState -> TodoState
toggleState state =
    case state of
        Open ->
            Done

        Done ->
            Open


toggleById : TodoId -> Todo -> Todo
toggleById id todo =
    if todo.id == id then
        { todo
            | state = toggleState todo.state
        }

    else
        todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ToggleTodo toggleId ->
            ( { model | todos = List.map (toggleById toggleId) model.todos }, Cmd.none )


renderTodo : Todo -> Html.Html Msg
renderTodo todo =
    div []
        [ div [] [ text todo.name ]
        , input
            [ attribute "type" "checkbox"
            , todo.state == Done |> checked
            , ToggleTodo todo.id |> onClick
            ]
            []
        ]


view : Model -> Html.Html Msg
view model =
    div [] (List.map renderTodo model.todos)
