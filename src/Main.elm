module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Events as Events
import Html as H
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Vector as Vector exposing (Vector)
import Random exposing (Generator, Seed)
import Svg as S
import Svg.Attributes as SA



-- SHIP


type alias Ship =
    { position : Vector
    , speed : Vector
    , angle : Float
    , rotatingLeft : Bool
    , rotatingRight : Bool
    , accelerating : Bool
    , pullingTrigger : Bool
    , gunState : GunState
    }


shipVertices : List Vector
shipVertices =
    [ ( 0, -12 ), ( -8, 12 ), ( 0, 8 ), ( 8, 12 ) ]


shipVerticesForCollisionDetection : List Vector
shipVerticesForCollisionDetection =
    repeatFirstVertex shipVertices


shipRadius : Float
shipRadius =
    boundingCircleRadius shipVertices


type GunState
    = Loaded
    | Reloading Float


initShip : Float -> Float -> Ship
initShip x y =
    { position = ( x, y )
    , speed = ( 0, 0 )
    , angle = 0
    , rotatingLeft = False
    , rotatingRight = False
    , accelerating = False
    , gunState = Loaded
    , pullingTrigger = False
    }


viewShip : Ship -> S.Svg Msg
viewShip { position, angle } =
    shipVertices
        |> List.map
            (\( x, y ) ->
                ( (cos angle * x) - (sin angle * y)
                , (sin angle * x) + (cos angle * y)
                )
            )
        |> List.map (Vector.add position)
        |> viewPolygon


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
                Vector.add ship.speed
                    ( sin newAngle * 175 * delta
                    , -(cos newAngle * 175 * delta)
                    )

            else if Vector.lengthSquared ship.speed > 1 then
                Vector.scale 0.99 ship.speed

            else
                ( 0, 0 )

        newPosition =
            Vector.add ship.position (Vector.scale delta newSpeed)
                |> (\( x, y ) ->
                        ( if x < 0 then
                            960

                          else if x > 960 then
                            0

                          else
                            x
                        , if y < 0 then
                            540

                          else if y > 540 then
                            0

                          else
                            y
                        )
                   )

        ( newGunState, bullet ) =
            case ( ship.gunState, ship.pullingTrigger ) of
                ( Loaded, True ) ->
                    ( Reloading 0.2, Just (initBullet ship) )

                ( Loaded, False ) ->
                    ( Loaded, Nothing )

                ( Reloading timeLeft, True ) ->
                    if timeLeft - delta <= 0 then
                        ( Reloading 0.2, Just (initBullet ship) )

                    else
                        ( Reloading (timeLeft - delta), Nothing )

                ( Reloading timeLeft, False ) ->
                    if timeLeft - delta <= 0 then
                        ( Loaded, Nothing )

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



-- BULLET


type alias Bullet =
    { position : Vector
    , angle : Float
    , rotation : Float
    }


bulletVertices : List Vector
bulletVertices =
    [ ( -3, -3 ), ( 3, -3 ), ( 3, 3 ), ( -3, 3 ) ]


bulletVerticesForCollisionDetection : List Vector
bulletVerticesForCollisionDetection =
    repeatFirstVertex bulletVertices


bulletRadius : Float
bulletRadius =
    boundingCircleRadius bulletVertices


initBullet : Ship -> Bullet
initBullet ship =
    { position = Vector.add ship.position (rotate ship.angle ( 0, -shipRadius ))
    , angle = ship.angle
    , rotation = 0
    }


viewBullet : Bullet -> S.Svg Msg
viewBullet { position, angle, rotation } =
    List.map (rotate rotation) bulletVertices
        |> List.map (rotate angle)
        |> List.map (Vector.add position)
        |> viewPolygon


updateBullet : Float -> Bullet -> Bullet
updateBullet delta bullet =
    { bullet
        | position =
            Vector.add bullet.position
                ( sin bullet.angle * 350 * delta
                , -(cos bullet.angle * 350 * delta)
                )
        , rotation = bullet.rotation + (delta * 12)
    }



-- ASTEROID


type alias Asteroid =
    { vertices : List Vector
    , verticesForCollisionDetection : List Vector
    , radius : Float
    , rotation : Float
    , rotationSpeed : Float
    , position : Vector
    , angle : Float
    }


viewAsteroid : Asteroid -> S.Svg msg
viewAsteroid asteroid =
    List.map (rotate asteroid.rotation) asteroid.vertices
        |> List.map (Vector.add asteroid.position)
        |> viewPolygon


updateAsteroid : Float -> Asteroid -> Asteroid
updateAsteroid delta asteroid =
    { asteroid
        | position =
            Vector.add asteroid.position
                ( sin asteroid.angle * 40 * delta
                , -(cos asteroid.angle * 40 * delta)
                )
        , rotation = asteroid.rotation + (delta * asteroid.rotationSpeed)
    }


asteroidGenerator : Vector -> Generator Asteroid
asteroidGenerator _ =
    let
        vertices =
            Random.list 8 (Random.int 25 40)
                |> Random.map
                    (List.indexedMap
                        (\index distance ->
                            let
                                r =
                                    toFloat distance

                                theta =
                                    degrees (toFloat (index * 45))
                            in
                            ( r * cos theta, r * sin theta )
                        )
                    )

        rotationSpeed =
            Random.float -1 1

        position =
            Random.map2 Vector.from (Random.float 0 960) (Random.float 0 540)

        angle =
            Random.map degrees (Random.float 0 360)
    in
    Random.map4 initAsteroid vertices rotationSpeed position angle


initAsteroid : List Vector -> Float -> Vector -> Float -> Asteroid
initAsteroid vertices rotationSpeed position angle =
    { vertices = vertices
    , verticesForCollisionDetection = repeatFirstVertex vertices
    , radius = boundingCircleRadius vertices
    , rotation = 0
    , rotationSpeed = rotationSpeed
    , position = position
    , angle = angle
    }



-- HELPER FUNCTIONS


rotate : Float -> Vector -> Vector
rotate rotation ( x, y ) =
    ( (cos rotation * x) - (sin rotation * y)
    , (sin rotation * x) + (cos rotation * y)
    )


viewPolygon : List Vector -> S.Svg msg
viewPolygon vertices =
    S.polygon
        [ SA.points (String.join " " (List.map toString vertices))
        , HA.style "stroke" "white"
        , HA.style "stroke-width" "2"
        ]
        []


toString : Vector -> String
toString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


boundingCircleRadius : List Vector -> Float
boundingCircleRadius =
    List.foldl
        (\vertex currentRadius ->
            let
                distance =
                    Vector.distance ( 0, 0 ) vertex
            in
            if distance > currentRadius then
                distance

            else
                currentRadius
        )
        0



-- TEA


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

        initialSeed =
            Decode.decodeValue (Decode.field "initialSeed" Decode.int) flags
                |> Result.withDefault 0
                |> Random.initialSeed

        ( asteroids, newSeed ) =
            asteroidGenerator ( width / 2, height / 2 )
                |> Random.list 10
                |> flip Random.step initialSeed
    in
    ( { width = width
      , height = height
      , ship = initShip (width / 2) (height / 2)
      , bullets = []
      , asteroids = asteroids
      , seed = newSeed
      }
    , Cmd.none
    )


view : Model -> H.Html Msg
view model =
    S.svg
        [ HA.width (round model.width)
        , HA.height (round model.height)
        , HA.style "border" "1px gray solid"
        , HA.style "position" "absolute"
        , HA.style "left" "50%"
        , HA.style "top" "50%"
        , HA.style "transform" "translate(-50%, -50%)"
        ]
        [ viewShip model.ship
        , S.g [] (List.map viewBullet model.bullets)
        , S.g [] (List.map viewAsteroid model.asteroids)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FramePassed delta ->
            let
                ( newShip, maybeBullet ) =
                    updateShip delta model.ship

                updatedBullets =
                    List.filter
                        (isInside
                            { top = -8
                            , right = model.width + 8
                            , bottom = model.height + 8
                            , left = -8
                            }
                        )
                        (case maybeBullet of
                            Just bullet ->
                                bullet :: model.bullets

                            Nothing ->
                                model.bullets
                        )
                        |> List.map (updateBullet delta)

                updatedAsteroids =
                    List.map
                        (\asteroid ->
                            let
                                newAsteroid =
                                    updateAsteroid delta asteroid

                                ( x, y ) =
                                    newAsteroid.position
                            in
                            { newAsteroid
                                | position =
                                    ( if x < -40 then
                                        1000

                                      else if x > 1000 then
                                        -40

                                      else
                                        x
                                    , if y < -40 then
                                        600

                                      else if y > 600 then
                                        -40

                                      else
                                        y
                                    )
                            }
                        )
                        model.asteroids

                ( newBullets, newAsteroids ) =
                    List.foldl
                        (\bullet ( accumulatedBullets, asteroids ) ->
                            case List.filter (isCollidingWith bullet) asteroids of
                                [] ->
                                    -- The bullet doesn't collide with any
                                    -- asteroid. Add it to the accumulated
                                    -- bullets list and leave the asteroid list
                                    -- intact.
                                    ( bullet :: accumulatedBullets, asteroids )

                                collidingAsteroids ->
                                    -- The bullet collides with some asteroids.
                                    -- Don't add it to the accumulated bullets
                                    -- list and remove the asteroids from the
                                    -- list.
                                    ( accumulatedBullets
                                    , List.filter
                                        (not << flip List.member collidingAsteroids)
                                        asteroids
                                    )
                        )
                        ( [], updatedAsteroids )
                        updatedBullets
            in
            ( { model
                | ship = newShip
                , bullets = newBullets
                , asteroids = newAsteroids
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
            in
            ( { model | ship = newShip }
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


repeatFirstVertex : List Vector -> List Vector
repeatFirstVertex polygon =
    case polygon of
        [] ->
            []

        first :: rest ->
            first :: rest ++ [ first ]


isCollidingWith : Bullet -> Asteroid -> Bool
isCollidingWith bullet asteroid =
    if
        isCircleInsideCircle
            ( bullet.position, bulletRadius )
            ( asteroid.position, asteroid.radius )
    then
        let
            transformedBulletVertices =
                bulletVerticesForCollisionDetection
                    |> List.map (rotate bullet.angle)
                    |> List.map (rotate bullet.rotation)
                    |> List.map (Vector.add bullet.position)

            transformedAsteroidVertices =
                asteroid.verticesForCollisionDetection
                    |> List.map (rotate asteroid.rotation)
                    |> List.map (Vector.add asteroid.position)

            verticesToEdges vertices =
                case vertices of
                    first :: second :: rest ->
                        ( first, second ) :: verticesToEdges (second :: rest)

                    _ ->
                        []

            isPointInPolygon ( px, py ) polygon =
                let
                    normals =
                        List.map
                            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                                Vector.crossProduct ( x2 - x1, y2 - y1 ) ( px - x1, py - y1 )
                            )
                            (verticesToEdges polygon)
                in
                List.all (flip (>=) 0) normals
        in
        List.foldl
            (\bulletVertex collided ->
                if collided then
                    True

                else
                    isPointInPolygon bulletVertex transformedAsteroidVertices
            )
            False
            transformedBulletVertices

    else
        False


isCircleInsideCircle : ( Vector, Float ) -> ( Vector, Float ) -> Bool
isCircleInsideCircle ( ( x1, y1 ), r1 ) ( ( x2, y2 ), r2 ) =
    ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) <= ((r1 + r2) ^ 2)


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
    -> { r | position : Vector }
    -> Bool
isInside { top, left, right, bottom } { position } =
    let
        ( x, y ) =
            position
    in
    x > left && x < right && y > top && y < bottom
