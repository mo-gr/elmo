module Boid exposing (main)

import Browser
import Browser.Events
import Html exposing (Attribute)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode
import String exposing (fromInt)
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
    makeBoidV2 { x = 0, y = 0 }


makeBoidV2 : V2 -> Boid
makeBoidV2 pos0 =
    Boid
        { pos = pos0
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
        |> keepDistance others
        |> alignWithFlock others
        |> clampForce 1
        |> clampVelocity 5


clampForce : Float -> Boid -> Boid
clampForce limit (Boid boid) =
    if vlength boid.force > limit then
        clampForce limit (Boid { boid | force = vscale 0.5 boid.force })

    else
        Boid boid


clampVelocity : Float -> Boid -> Boid
clampVelocity limit (Boid boid) =
    if vlength boid.velocity > limit then
        clampVelocity limit (Boid { boid | velocity = vscale 0.5 boid.velocity })

    else
        Boid boid


resetForce : Boid -> Boid
resetForce (Boid boid) =
    Boid { boid | force = { x = 0, y = 0 } }


flyToNeighbours : List Boid -> Boid -> Boid
flyToNeighbours neighbours (Boid boid) =
    if List.isEmpty neighbours then
        Boid boid

    else
        let
            visibleNeighbours =
                List.filter
                    (\other -> distance (Boid boid) other < 200 && distance (Boid boid) other > 20)
                    neighbours
        in
        if List.isEmpty visibleNeighbours then
            Boid boid

        else
            visibleNeighbours
                |> List.foldr
                    (\(Boid other) acc -> vadd acc other.pos)
                    { x = 0, y = 0 }
                |> vscale (1 / toFloat (List.length visibleNeighbours))
                |> vadd (vscale -1 boid.pos)
                |> vscale (1 / 100)
                |> (\flockForce -> Boid { boid | force = vadd boid.force flockForce })


keepDistance : List Boid -> Boid -> Boid
keepDistance neighbours (Boid boid) =
    if List.isEmpty neighbours then
        Boid boid

    else
        neighbours
            |> List.filter (\other -> distance (Boid boid) other < 15)
            |> List.foldr
                (\(Boid other) avoid -> vsub avoid (vsub boid.pos other.pos))
                { x = 0, y = 0 }
            |> vscale (1 / 8)
            |> (\avoidanceForce -> Boid { boid | force = vadd boid.force avoidanceForce })


alignWithFlock : List Boid -> Boid -> Boid
alignWithFlock neighbours (Boid boid) =
    if List.isEmpty neighbours then
        Boid boid

    else
        let
            visibleNeighbours =
                List.filter (\other -> distance (Boid boid) other < 50) neighbours
        in
        if List.isEmpty visibleNeighbours then
            Boid boid

        else
            visibleNeighbours
                |> List.foldr
                    (\(Boid other) commonV -> vadd commonV (vadd boid.velocity other.velocity))
                    { x = 0, y = 0 }
                |> vscale (1 / toFloat (List.length visibleNeighbours))
                |> vscale (1 / 8)
                |> (\commonVelocity -> Boid { boid | velocity = vadd commonVelocity boid.velocity })


pos : Boid -> V2
pos (Boid b) =
    b.pos


distance : Boid -> Boid -> Float
distance (Boid b1) (Boid b2) =
    vsub b1.pos b2.pos |> vlength


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


vsub : V2 -> V2 -> V2
vsub v1 v2 =
    vscale -1 v1 |> vadd v2


vscale : Float -> V2 -> V2
vscale n v =
    { x = v.x * n, y = v.y * n }


vlength : V2 -> Float
vlength { x, y } =
    sqrt (x * x + y * y)


onClickWithCoord : (V2 -> Msg) -> Attribute Msg
onClickWithCoord makeMessage =
    Json.Decode.map2 (\x y -> makeMessage { x = x, y = y })
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)
        |> on "click"


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
            , onClickWithCoord (makeBoidV2 >> SpawnBoid)
            ]
            (List.map drawBoid model.boids)
        ]


drawBoid : Boid -> Svg.Svg Msg
drawBoid (Boid boid) =
    Svg.circle
        [ Svg.Attributes.cx (boid.pos.x |> round |> modBy 500 |> fromInt)
        , Svg.Attributes.cy (boid.pos.y |> round |> modBy 500 |> fromInt)
        , Svg.Attributes.r "5"
        ]
        []
