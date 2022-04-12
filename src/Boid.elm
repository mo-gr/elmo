module Boid exposing (main)

import Browser
import Browser.Events
import Html
import Html.Attributes
import Svg
import Svg.Attributes


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
   Browser.Events.onAnimationFrameDelta AnimationFrame


type Msg
    = AnimationFrame Float


type alias Model =
    { boids : List Boid
    , world : { width : Int, height : Int }
    }


type Boid
    = Boid { pos : V2, velocity : V2, force : V2 }


type alias V2 =
    { x : Float, y : Float }


makeBoid : Boid
makeBoid =
    Boid
        { pos = { x = 0, y = 0 }
        , velocity = { x = 1, y = 1 }
        , force = { x = 0, y = 0 }
        }


init : a -> ( Model, Cmd msg )
init _ =
    ( { boids = List.singleton makeBoid
      , world = { width = 500, height = 500 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { model | boids = List.map updateBoid model.boids }, Cmd.none )


updateBoid : Boid -> Boid
updateBoid (Boid b) =
    Boid
        { b
            | pos = vadd b.pos b.velocity
            , velocity = vadd b.velocity b.force
        }


vadd : V2 -> V2 -> V2
vadd v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        ]
        [ Svg.svg
            [ Svg.Attributes.width (String.fromInt model.world.width)
            , Svg.Attributes.height (String.fromInt model.world.height)
            ]
            (List.map drawBoid model.boids)
        ]


drawBoid : Boid -> Svg.Svg msg
drawBoid (Boid boid) =
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat boid.pos.x)
        , Svg.Attributes.cy (String.fromFloat boid.pos.y)
        , Svg.Attributes.r "5"
        ]
        []
