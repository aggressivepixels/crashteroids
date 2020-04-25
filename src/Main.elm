module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Events as Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes


type alias Ship =
    { position : Vec2
    , speed : Vec2
    , angle : Float
    , rotatingLeft : Bool
    , rotatingRight : Bool
    , accelerating : Bool
    , pullingTrigger : Bool
    , gunState : GunState
    }


type GunState
    = Idle
    | Reloading Float


initShip : Float -> Float -> Ship
initShip x y =
    { position = vec2 x y
    , speed = vec2 0 0
    , angle = 0
    , rotatingLeft = False
    , rotatingRight = False
    , accelerating = False
    , gunState = Idle
    , pullingTrigger = False
    }


updateShip : Float -> Ship -> ( Ship, Maybe Bullet )
updateShip delta ship =
    let
        newAngle =
            case ( ship.rotatingLeft, ship.rotatingRight ) of
                ( True, False ) ->
                    ship.angle - (4 * delta)

                ( False, True ) ->
                    ship.angle + (4 * delta)

                _ ->
                    ship.angle

        newSpeed =
            if ship.accelerating then
                Vec2.add ship.speed <|
                    Vec2.fromRecord
                        { x = sin newAngle * 175 * delta
                        , y = negate (cos newAngle * 175 * delta)
                        }

            else if Vec2.lengthSquared ship.speed > 1 then
                Vec2.scale 0.99 ship.speed

            else
                vec2 0 0

        newPosition =
            Vec2.add ship.position (Vec2.scale delta newSpeed)
                |> Vec2.toRecord
                |> (\{ x, y } ->
                        vec2
                            (if x < 0 then
                                960

                             else if x > 960 then
                                0

                             else
                                x
                            )
                            (if y < 0 then
                                540

                             else if y > 540 then
                                0

                             else
                                y
                            )
                   )

        ( newGunState, bullet ) =
            case ( ship.gunState, ship.pullingTrigger ) of
                ( Idle, True ) ->
                    ( Reloading 0.2, Just (initBullet ship) )

                ( Idle, False ) ->
                    ( Idle, Nothing )

                ( Reloading timeLeft, True ) ->
                    if timeLeft - delta <= 0 then
                        ( Reloading 0.2, Just (initBullet ship) )

                    else
                        ( Reloading (timeLeft - delta), Nothing )

                ( Reloading timeLeft, False ) ->
                    if timeLeft - delta <= 0 then
                        ( Idle, Nothing )

                    else
                        ( Reloading (timeLeft - delta), Nothing )
    in
    ( { ship
        | angle = newAngle
        , speed = newSpeed
        , position = newPosition
        , gunState = newGunState
      }
    , bullet
    )


viewShip : Ship -> Svg Msg
viewShip { position, angle } =
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
                            { x = (cos angle * x) - (sin angle * y)
                            , y = (sin angle * x) + (cos angle * y)
                            }
                    )
                |> List.map (Vec2.add position)
    in
    Svg.g [] [ viewPolygon transformed ]


type alias Bullet =
    { position : Vec2
    , angle : Float
    , rotation : Float
    }


initBullet : Ship -> Bullet
initBullet ship =
    { position =
        rotate ship.angle (vec2 0 -12)
            |> Vec2.add ship.position
    , angle = ship.angle
    , rotation = 0
    }


updateBullet : Float -> Bullet -> Bullet
updateBullet delta bullet =
    { bullet
        | position =
            Vec2.add bullet.position
                (Vec2.fromRecord
                    { x = sin bullet.angle * 350 * delta
                    , y = negate (cos bullet.angle * 350 * delta)
                    }
                )
        , rotation = bullet.rotation + (delta * 12)
    }


viewBullet : Bullet -> Svg Msg
viewBullet { position, angle, rotation } =
    let
        vertices =
            [ vec2 -3 -3, vec2 3 -3, vec2 3 3, vec2 -3 3 ]

        transformed =
            vertices
                |> List.map (rotate rotation)
                |> List.map (rotate angle)
                |> List.map (Vec2.add position)
    in
    Svg.g [] [ viewPolygon transformed ]


type alias Asteroid =
    { vertices : List Vec2
    , rotation : Float
    , position : Vec2
    , rotationSpeed : Float
    }


rotate : Float -> Vec2 -> Vec2
rotate rotation v =
    let
        { x, y } =
            Vec2.toRecord v
    in
    Vec2.fromRecord
        { x = (cos rotation * x) - (sin rotation * y)
        , y = (sin rotation * x) + (cos rotation * y)
        }


viewPolygon : List Vec2 -> Svg msg
viewPolygon vertices =
    Svg.polygon
        [ Svg.Attributes.points (String.join " " (List.map toString vertices))
        , Html.Attributes.style "stroke" "white"
        , Html.Attributes.style "stroke-width" "2"
        ]
        []


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
    , bullets : List Bullet
    , asteroids : List Asteroid
    , seed : Seed
    }


type Msg
    = FramePassed Float
    | KeyPressed String
    | KeyReleased String


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( width, height ) =
            ( 960, 540 )
    in
    ( { width = width
      , height = height
      , ship = initShip (width / 2) (height / 2)
      , bullets = []
      , asteroids = []
      , seed =
            Decode.decodeValue (Decode.field "initialSeed" Decode.int) flags
                |> Result.withDefault 0
                |> Random.initialSeed
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
        [ viewShip model.ship
        , Svg.g [] (List.map viewBullet model.bullets)
        , Svg.g [] (List.map viewAsteroid model.asteroids)
        ]


updateAsteroid : Float -> Asteroid -> Asteroid
updateAsteroid delta a =
    { a | rotation = a.rotation + (delta * a.rotationSpeed) }


viewAsteroid : Asteroid -> Html msg
viewAsteroid a =
    List.map (rotate a.rotation) a.vertices
        |> List.map (Vec2.add a.position)
        |> viewPolygon


asteroidGenerator : Vec2 -> Generator Asteroid
asteroidGenerator position =
    let
        verticesGenerator =
            Random.list 8 (Random.int 25 40)

        rotationSpeedGenerator =
            Random.float -0.8 0.8
    in
    Random.map2
        (\distances rotationSpeed ->
            { vertices =
                List.indexedMap
                    (\index distance ->
                        let
                            r =
                                toFloat distance

                            theta =
                                degrees (toFloat (index * 45))
                        in
                        vec2 (r * cos theta) (r * sin theta)
                    )
                    distances
            , position = position
            , rotation = 0
            , rotationSpeed = rotationSpeed
            }
        )
        verticesGenerator
        rotationSpeedGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FramePassed delta ->
            let
                ( newShip, maybeBullet ) =
                    updateShip delta model.ship

                newBullets =
                    List.filter
                        (isInside
                            { top = -8
                            , right = model.width + 8
                            , bottom = model.height + 8
                            , left = -8
                            }
                        )
                    <|
                        case maybeBullet of
                            Just bullet ->
                                bullet :: model.bullets

                            Nothing ->
                                model.bullets
            in
            ( { model
                | ship = newShip
                , bullets = List.map (updateBullet delta) newBullets
                , asteroids = List.map (updateAsteroid delta) model.asteroids
              }
            , Cmd.none
            )

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

                        " " ->
                            { ship | pullingTrigger = True }

                        _ ->
                            ship

                ( newAsteroids, newSeed ) =
                    if key == "z" then
                        asteroidGenerator (vec2 (model.width / 2) (model.height / 2))
                            |> Random.map (flip (::) model.asteroids)
                            |> flip Random.step model.seed

                    else
                        ( model.asteroids, model.seed )
            in
            ( { model
                | ship = newShip
                , asteroids = newAsteroids
                , seed = newSeed
              }
            , Cmd.none
            )

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

                        " " ->
                            { ship | pullingTrigger = False }

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


isInside :
    { top : Float, left : Float, right : Float, bottom : Float }
    -> { r | position : Vec2 }
    -> Bool
isInside { top, left, right, bottom } { position } =
    let
        { x, y } =
            Vec2.toRecord position
    in
    x > left && x < right && y > top && y < bottom
