module Todo exposing (main)

import Browser
import Html
import Html.Attributes exposing (attribute, checked, placeholder)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { todos : List Todo
    , newTodo : String
    }


type alias Todo =
    { name : String
    , status : Status
    }


type Status
    = Open
    | Done


type Msg
    = CreateTodo
    | ChangeNewTodo String
    | ToggleTodo Int
    | ClearDone


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init _ =
    ( { todos = [ { name = "Learn elm", status = Open } ]
      , newTodo = ""
      }
    , Cmd.none
    )


subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        CreateTodo ->
            ( { model
                | todos = model.todos ++ [ { name = model.newTodo, status = Open } ]
                , newTodo = ""
              }
            , Cmd.none
            )

        ChangeNewTodo newTodo ->
            ( { model | newTodo = newTodo }, Cmd.none )

        ToggleTodo idx ->
            ( { model | todos = List.indexedMap (toggleByIndex idx) model.todos }, Cmd.none )

        ClearDone ->
            ( { model | todos = List.filter (\todo -> todo.status == Open) model.todos }, Cmd.none )


toggleByIndex : Int -> Int -> Todo -> Todo
toggleByIndex target idx todo =
    if target == idx then
        { todo
            | status =
                case todo.status of
                    Done ->
                        Open

                    Open ->
                        Done
        }

    else
        todo


renderTodo : Int -> Todo -> Html.Html Msg
renderTodo idx todo =
    Html.div []
        [ Html.input
            [ attribute "type" "checkbox"
            , attribute "value" ""
            , onClick (ToggleTodo idx)
            , checked (todo.status == Done)
            ]
            []
        , Html.text todo.name
        ]


renderCreate : Model -> Html.Html Msg
renderCreate model =
    Html.div []
        [ Html.input
            [ placeholder "Enter new Todo"
            , onInput ChangeNewTodo
            ]
            [ Html.text model.newTodo ]
        , Html.button
            [ onClick CreateTodo ]
            [ Html.text "Create Todo" ]
        ]


renderClear : Html.Html Msg
renderClear =
    Html.div []
        [ Html.button [ onClick ClearDone ] [ Html.text "Clear Done" ] ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        (List.indexedMap renderTodo model.todos ++ [ renderCreate model, renderClear ])
