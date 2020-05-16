module Main exposing (main)

import Basics.Extra exposing (flip, fmodBy)
import Browser
import Browser.Events as Events
import Html as H
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Vector as Vector exposing (Vector)
import Random exposing (Generator, Seed)
import Random.Extra
import Svg as S
import Svg.Attributes as SA


type alias Bounded r =
    { r
        | width : Float
        , height : Float
    }



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


type GunState
    = Loaded
    | Reloading Float


shipVertices : List Vector
shipVertices =
    [ ( 0, -12 ), ( -8, 12 ), ( 0, 8 ), ( 8, 12 ) ]


shipVerticesForCollisionDetection : List Vector
shipVerticesForCollisionDetection =
    repeatFirstVertex shipVertices


shipRadius : Float
shipRadius =
    boundingCircleRadius shipVertices


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


updateShip : Bounded r -> Float -> Ship -> ( Ship, Maybe Bullet )
updateShip bounds delta ship =
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
                |> Tuple.mapBoth
                    (wrapBetween 0 bounds.width)
                    (wrapBetween 0 bounds.height)

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
                ( sin bullet.angle * 500 * delta
                , -(cos bullet.angle * 500 * delta)
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
    , size : Size
    , speed : Float
    }


type Size
    = Large
    | Medium
    | Small


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
                ( sin asteroid.angle * asteroid.speed * delta
                , -(cos asteroid.angle * asteroid.speed * delta)
                )
        , rotation = asteroid.rotation + (delta * asteroid.rotationSpeed)
    }


asteroidAngleGenerator : Generator Float
asteroidAngleGenerator =
    Random.map degrees (Random.float 0 360)


asteroidRotationSpeedGenerator : Generator Float
asteroidRotationSpeedGenerator =
    Random.float -1 1


asteroidPositionGenerator : Bounded r -> Generator Vector
asteroidPositionGenerator bounds =
    Random.map2 Vector.from
        (Random.float 0 bounds.width)
        (Random.float 0 bounds.height)


asteroidVerticesGenerator : Size -> Generator (List Vector)
asteroidVerticesGenerator size =
    let
        ( vertexCount, ( low, high ) ) =
            case size of
                Large ->
                    ( 8, ( 25, 40 ) )

                Medium ->
                    ( 6, ( 15, 25 ) )

                Small ->
                    ( 5, ( 6, 12 ) )
    in
    Random.list vertexCount (Random.int low high)
        |> Random.map
            (List.indexedMap
                (\index distance ->
                    let
                        r =
                            toFloat distance

                        theta =
                            degrees (toFloat index * (360 / toFloat vertexCount))
                    in
                    ( r * cos theta, r * sin theta )
                )
            )


initAsteroid : List Vector -> Float -> Vector -> Float -> Size -> Asteroid
initAsteroid vertices rotationSpeed position angle size =
    { vertices = vertices
    , verticesForCollisionDetection = repeatFirstVertex vertices
    , radius = boundingCircleRadius vertices
    , rotation = 0
    , rotationSpeed = rotationSpeed
    , position = position
    , angle = angle
    , size = size
    , speed =
        case size of
            Small ->
                100

            Medium ->
                75

            Large ->
                40
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
    | KeyPressed Key
    | KeyReleased Key


type Key
    = Left
    | Up
    | Right
    | Space


keyFromString : String -> Maybe Key
keyFromString s =
    case String.toLower s of
        "a" ->
            Just Left

        "w" ->
            Just Up

        "d" ->
            Just Right

        " " ->
            Just Space

        _ ->
            Nothing


keyDecoder : (Key -> msg) -> Decoder msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                case keyFromString s of
                    Just key ->
                        Decode.succeed (toMsg key)

                    Nothing ->
                        Decode.fail ("Not interested in " ++ s)
            )


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
            Random.map5 initAsteroid
                (asteroidVerticesGenerator Large)
                asteroidRotationSpeedGenerator
                (asteroidPositionGenerator { width = width, height = height })
                asteroidAngleGenerator
                (Random.constant Large)
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
                    updateShip model delta model.ship

                updatedBullets =
                    List.filter
                        (isInside
                            { top = bulletRadius
                            , right = model.width + bulletRadius
                            , bottom = model.height + bulletRadius
                            , left = -bulletRadius
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
                            in
                            { newAsteroid
                                | position =
                                    Tuple.mapBoth
                                        (wrapBetween
                                            -newAsteroid.radius
                                            (model.width + newAsteroid.radius)
                                        )
                                        (wrapBetween
                                            -newAsteroid.radius
                                            (model.height + newAsteroid.radius)
                                        )
                                        newAsteroid.position
                            }
                        )
                        model.asteroids

                ( newBullets, newAsteroids, newSeed ) =
                    List.foldl
                        (\bullet ( accumulatedBullets, asteroids, seed ) ->
                            case List.filter (isCollidingWith bullet) asteroids of
                                [] ->
                                    ( bullet :: accumulatedBullets, asteroids, seed )

                                collidingAsteroids ->
                                    let
                                        ( accumulatedAsteroids, accumulatedSeed ) =
                                            processAsteroidCollision
                                                bullet
                                                collidingAsteroids
                                                seed
                                                asteroids
                                    in
                                    ( accumulatedBullets
                                    , accumulatedAsteroids
                                    , accumulatedSeed
                                    )
                        )
                        ( [], updatedAsteroids, model.seed )
                        updatedBullets
            in
            ( { model
                | ship = newShip
                , bullets = newBullets
                , asteroids = newAsteroids
                , seed = newSeed
              }
            , Cmd.none
            )

        KeyPressed key ->
            let
                ship =
                    model.ship

                newShip =
                    case key of
                        Left ->
                            { ship | rotatingLeft = True }

                        Up ->
                            { ship | accelerating = True }

                        Right ->
                            { ship | rotatingRight = True }

                        Space ->
                            { ship | pullingTrigger = True }
            in
            ( { model | ship = newShip }, Cmd.none )

        KeyReleased key ->
            let
                ship =
                    model.ship

                newShip =
                    case key of
                        Left ->
                            { ship | rotatingLeft = False }

                        Up ->
                            { ship | accelerating = False }

                        Right ->
                            { ship | rotatingRight = False }

                        Space ->
                            { ship | pullingTrigger = False }
            in
            ( { model | ship = newShip }, Cmd.none )


processAsteroidCollision :
    Bullet
    -> List Asteroid
    -> Seed
    -> List Asteroid
    -> ( List Asteroid, Seed )
processAsteroidCollision bullet collidingAsteroids seed =
    let
        makeAsteroid position size angle =
            Random.map5 initAsteroid
                (asteroidVerticesGenerator size)
                asteroidRotationSpeedGenerator
                (Random.constant position)
                (Random.constant (bullet.angle + angle))
                (Random.constant size)

        makeAsteroids position size =
            List.map (makeAsteroid position size) [ -45, 0, 45 ]
    in
    List.concatMap
        (\asteroid ->
            if List.member asteroid collidingAsteroids then
                case asteroid.size of
                    Large ->
                        makeAsteroids asteroid.position Medium

                    Medium ->
                        makeAsteroids asteroid.position Small

                    Small ->
                        []

            else
                [ Random.constant asteroid ]
        )
        >> Random.Extra.sequence
        >> flip Random.step seed


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
        in
        isCollidingWithHelp transformedBulletVertices transformedAsteroidVertices

    else
        False


isCollidingWithHelp : List Vector -> List Vector -> Bool
isCollidingWithHelp transformedBulletVertices transformedAsteroidVertices =
    case transformedBulletVertices of
        [] ->
            False

        bulletVertex :: otherBulletVertices ->
            if isPointInPolygon bulletVertex transformedAsteroidVertices then
                True

            else
                isCollidingWithHelp otherBulletVertices transformedAsteroidVertices


verticesToEdges : List Vector -> List ( Vector, Vector )
verticesToEdges vertices =
    case vertices of
        first :: second :: rest ->
            ( first, second ) :: verticesToEdges (second :: rest)

        _ ->
            []


isPointInPolygon : Vector -> List Vector -> Bool
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


isCircleInsideCircle : ( Vector, Float ) -> ( Vector, Float ) -> Bool
isCircleInsideCircle ( ( x1, y1 ), r1 ) ( ( x2, y2 ), r2 ) =
    ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) <= ((r1 + r2) ^ 2)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta (FramePassed << flip (/) 1000)
        , Events.onKeyDown (keyDecoder KeyPressed)
        , Events.onKeyUp (keyDecoder KeyReleased)
        ]


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


wrapBetween : Float -> Float -> Float -> Float
wrapBetween low high value =
    fmodBy (high - low) (value - low) + low
