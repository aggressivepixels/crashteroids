module Main exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (Svg)
import Svg.Attributes


type alias Ship =
    { position : Vec2
    , speed : Vec2
    , rotation : Float
    , rotatingLeft : Bool
    , rotatingRight : Bool
    , accelerating : Bool
    }


initShip : Float -> Float -> Ship
initShip x y =
    { position = vec2 x y
    , speed = vec2 0 0
    , rotation = 0
    , rotatingLeft = False
    , rotatingRight = False
    , accelerating = False
    }


updateShip : Float -> Ship -> Ship
updateShip delta ship =
    let
        newRotation =
            case ( ship.rotatingLeft, ship.rotatingRight ) of
                ( True, False ) ->
                    ship.rotation - (4 * delta)

                ( False, True ) ->
                    ship.rotation + (4 * delta)

                _ ->
                    ship.rotation

        newSpeed =
            if ship.accelerating then
                Vec2.add ship.speed <|
                    Vec2.fromRecord
                        { x = sin newRotation * 175 * delta
                        , y = negate (cos newRotation * 175 * delta)
                        }

            else if Vec2.lengthSquared ship.speed > 1 then
                Vec2.scale 0.99 ship.speed

            else
                vec2 0 0

        newPosition =
            Vec2.add ship.position (Vec2.scale delta newSpeed)
    in
    { ship
        | rotation = newRotation
        , speed = newSpeed
        , position = newPosition
    }


viewShip : Ship -> Svg Msg
viewShip { position, rotation } =
    let
        vertices =
            [ vec2 0 -12, vec2 -8 12, vec2 0 8, vec2 8 12 ]

        transformed =
            vertices
                |> List.map
                    (\v ->
                        let
                            { x, y } =
                                Vec2.toRecord v
                        in
                        Vec2.fromRecord
                            { x = (cos rotation * x) - (sin rotation * y)
                            , y = (sin rotation * x) + (cos rotation * y)
                            }
                    )
                |> List.map (Vec2.add position)
    in
    Svg.g []
        [ viewPolygon transformed
        , Svg.g [] (List.map viewPolygon (buildGhosts transformed))
        ]


viewPolygon : List Vec2 -> Svg msg
viewPolygon vertices =
    Svg.polygon
        [ Svg.Attributes.points (String.join " " (List.map toString vertices))
        , Html.Attributes.style "stroke" "white"
        , Html.Attributes.style "stroke-width" "2"
        ]
        []


buildGhosts : List Vec2 -> List (List Vec2)
buildGhosts vertices =
    [ List.map (Vec2.add (vec2 960 0)) vertices
    , List.map (Vec2.add (vec2 960 -540)) vertices
    , List.map (Vec2.add (vec2 0 -540)) vertices
    , List.map (Vec2.add (vec2 -960 -540)) vertices
    , List.map (Vec2.add (vec2 -960 0)) vertices
    , List.map (Vec2.add (vec2 -960 540)) vertices
    , List.map (Vec2.add (vec2 0 540)) vertices
    , List.map (Vec2.add (vec2 960 540)) vertices
    ]


toString : Vec2 -> String
toString v =
    let
        { x, y } =
            Vec2.toRecord v
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


type alias Model =
    { width : Float
    , height : Float
    , ship : Ship
    }


type Msg
    = FramePassed Float
    | KeyPressed String
    | KeyReleased String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( width, height ) =
            ( 960, 540 )
    in
    ( { width = width
      , height = height
      , ship = initShip (width / 2) (height / 2)
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Svg.svg
        [ Html.Attributes.width (round model.width)
        , Html.Attributes.height (round model.height)
        , Html.Attributes.style "border" "1px gray solid"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "50%"
        , Html.Attributes.style "top" "50%"
        , Html.Attributes.style "transform" "translate(-50%, -50%)"
        ]
        [ viewShip model.ship ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FramePassed delta ->
            ( { model | ship = updateShip delta model.ship }, Cmd.none )

        KeyPressed key ->
            let
                ship =
                    model.ship

                newShip =
                    case key of
                        "a" ->
                            { ship | rotatingLeft = True }

                        "d" ->
                            { ship | rotatingRight = True }

                        "w" ->
                            { ship | accelerating = True }

                        _ ->
                            ship
            in
            ( { model | ship = newShip }, Cmd.none )

        KeyReleased key ->
            let
                ship =
                    model.ship

                newShip =
                    case key of
                        "a" ->
                            { ship | rotatingLeft = False }

                        "d" ->
                            { ship | rotatingRight = False }

                        "w" ->
                            { ship | accelerating = False }

                        _ ->
                            ship
            in
            ( { model | ship = newShip }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta (\delta -> FramePassed (delta / 1000))
        , Events.onKeyDown (keyDecoder KeyPressed)
        , Events.onKeyUp (keyDecoder KeyReleased)
        ]


keyDecoder : (String -> msg) -> Decoder msg
keyDecoder toMsg =
    Decode.map toMsg
        (Decode.field "key" Decode.string)
