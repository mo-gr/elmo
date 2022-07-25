module Boid exposing (main)

import Browser
import Browser.Events
import Html
import Html.Attributes
import Html.Events
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
    | SpawnBoid Boid


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
        , force = { x = -0.001, y = -0.001 }
        }


init : a -> ( Model, Cmd Msg )
init _ =
    { boids = []
    , world = { width = 500, height = 500 }
    }
        |> update (SpawnBoid makeBoid)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpawnBoid b ->
            ( { model | boids = b :: model.boids }, Cmd.none )

        AnimationFrame _ ->
            ( { model | boids = List.map (calculateForces model.boids >> updateBoid) model.boids }, Cmd.none )


calculateForces : List Boid -> Boid -> Boid
calculateForces bs b =
    let
        ( _, others ) =
            List.partition (\boid -> b == boid) bs
    in
    resetForce b
        |> flyToNeighbours others
        |> clampForce 1


clampForce : Float -> Boid -> Boid
clampForce limit (Boid boid) =
    if vlength boid.force > limit then
        clampForce limit (Boid {boid | force = vscale 0.5 boid.force})

    else
        Boid boid


resetForce : Boid -> Boid
resetForce (Boid boid) =
    Boid { boid | force = { x = 0, y = 0 } }


flyToNeighbours : List Boid -> Boid -> Boid
flyToNeighbours neighbours (Boid boid) =
    if List.length neighbours < 1 then
        Boid boid

    else
        let
            neighbourDraw =
                List.foldr vadd { x = 0, y = 0 } (List.map pos neighbours)
                    |> vscale (1 / toFloat (List.length neighbours))
                    |> vadd (vscale -1 boid.pos)
                    |> vscale (1 / 100)
        in
        Boid { boid | force = vadd boid.force neighbourDraw }


pos : Boid -> V2
pos (Boid b) =
    b.pos


updateBoid : Boid -> Boid
updateBoid (Boid b) =
    Boid
        { b
            | pos = vadd b.pos b.velocity
            , velocity = vadd b.velocity b.force |> vscale 0.99
        }


vadd : V2 -> V2 -> V2
vadd v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


vscale : Float -> V2 -> V2
vscale n v =
    { x = v.x * n, y = v.y * n }

vlength : V2 -> Float
vlength {x, y} = sqrt (x * x + y * y)

view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        , Html.Events.onClick (SpawnBoid makeBoid)
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
