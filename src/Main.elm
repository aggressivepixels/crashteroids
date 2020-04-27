module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Events as Events
import Html as H
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random exposing (Generator, Seed)
import Svg as S
import Svg.Attributes as SA



-- SHIP


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
    = Loaded
    | Reloading Float


initShip : Float -> Float -> Ship
initShip x y =
    { position = vec2 x y
    , speed = vec2 0 0
    , angle = 0
    , rotatingLeft = False
    , rotatingRight = False
    , accelerating = False
    , gunState = Loaded
    , pullingTrigger = False
    }


viewShip : Ship -> S.Svg Msg
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
    viewPolygon transformed


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


viewBullet : Bullet -> S.Svg Msg
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
    S.g [] [ viewPolygon transformed ]


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



-- ASTEROID


type alias Asteroid =
    { vertices : List Vec2
    , rotation : Float
    , rotationSpeed : Float
    , position : Vec2
    , angle : Float
    }


viewAsteroid : Asteroid -> S.Svg msg
viewAsteroid asteroid =
    List.map (rotate asteroid.rotation) asteroid.vertices
        |> List.map (Vec2.add asteroid.position)
        |> viewPolygon


updateAsteroid : Float -> Asteroid -> Asteroid
updateAsteroid delta asteroid =
    { asteroid
        | position =
            Vec2.add asteroid.position
                (Vec2.fromRecord
                    { x = sin asteroid.angle * 40 * delta
                    , y = negate (cos asteroid.angle * 40 * delta)
                    }
                )
        , rotation = asteroid.rotation + (delta * asteroid.rotationSpeed)
    }


asteroidGenerator : Vec2 -> Generator Asteroid
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
                            vec2 (r * cos theta) (r * sin theta)
                        )
                    )

        rotation =
            Random.constant 0

        rotationSpeed =
            Random.float -1 1

        position =
            Random.map2 vec2 (Random.float 0 960) (Random.float 0 540)

        angle =
            Random.map degrees (Random.float 0 360)
    in
    Random.map5 Asteroid vertices rotation rotationSpeed position angle



-- HELPER FUNCTIONS


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


viewPolygon : List Vec2 -> S.Svg msg
viewPolygon vertices =
    S.polygon
        [ SA.points (String.join " " (List.map toString vertices))
        , HA.style "stroke" "white"
        , HA.style "stroke-width" "2"
        ]
        []


toString : Vec2 -> String
toString v =
    let
        { x, y } =
            Vec2.toRecord v
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


boundingCircleRadius : Vec2 -> List Vec2 -> Float
boundingCircleRadius center =
    List.foldl
        (\vertex currentRadius ->
            let
                distance =
                    Vec2.distance center vertex
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
            asteroidGenerator (vec2 (width / 2) (height / 2))
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

                                { x, y } =
                                    Vec2.toRecord newAsteroid.position
                            in
                            { newAsteroid
                                | position =
                                    Vec2.fromRecord
                                        { x =
                                            if x < -40 then
                                                1000

                                            else if x > 1000 then
                                                -40

                                            else
                                                x
                                        , y =
                                            if y < -40 then
                                                600

                                            else if y > 600 then
                                                -40

                                            else
                                                y
                                        }
                            }
                        )
                        model.asteroids

                ( newBullets, newAsteroids ) =
                    updateBulletsAndAsteroids updatedBullets updatedAsteroids
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


updateBulletsAndAsteroids :
    List Bullet
    -> List Asteroid
    -> ( List Bullet, List Asteroid )
updateBulletsAndAsteroids =
    updateBulletsAndAsteroidsHelp []


updateBulletsAndAsteroidsHelp :
    List Bullet
    -> List Bullet
    -> List Asteroid
    -> ( List Bullet, List Asteroid )
updateBulletsAndAsteroidsHelp accumulatedBullets pendingBullets asteroids =
    case pendingBullets of
        [] ->
            -- No more bullets to process.
            ( accumulatedBullets, asteroids )

        bullet :: otherBullets ->
            -- Check if the bullet collides with one (or some) asteroid(s).
            case List.filter (isCollidingWith bullet) asteroids of
                [] ->
                    -- The bullet doesn't collide with any asteroid.
                    -- Add it to the accumulated bullets list and leave the
                    -- asteroid list intact.
                    updateBulletsAndAsteroidsHelp
                        (bullet :: accumulatedBullets)
                        otherBullets
                        asteroids

                collidingAsteroids ->
                    -- The bullet collides with one (or some) asteroid(s).
                    -- Don't add it to the accumulated bullets list and remove
                    -- the asteroid(s) from the list.
                    updateBulletsAndAsteroidsHelp
                        accumulatedBullets
                        otherBullets
                        (List.filter
                            (not << flip List.member collidingAsteroids)
                            asteroids
                        )


isCollidingWith : Bullet -> Asteroid -> Bool
isCollidingWith bullet asteroid =
    if
        isCircleInsideCircle
            ( bullet.position, 3 )
            ( asteroid.position
            , boundingCircleRadius (vec2 0 0) asteroid.vertices
            )
    then
        let
            bulletVertices =
                [ vec2 -3 -3, vec2 3 -3, vec2 3 3, vec2 -3 3, vec2 -3 -3 ]
                    |> List.map (rotate bullet.angle)
                    |> List.map (rotate bullet.rotation)
                    |> List.map (Vec2.add bullet.position)

            asteroidVertices =
                (asteroid.vertices
                    ++ [ List.head asteroid.vertices
                            |> Maybe.withDefault (vec2 0 0)
                       ]
                )
                    |> List.map (rotate asteroid.rotation)
                    |> List.map (Vec2.add asteroid.position)

            crossProduct v1 v2 =
                let
                    ( x1, x2 ) =
                        ( Vec2.getX v1, Vec2.getX v2 )

                    ( y1, y2 ) =
                        ( Vec2.getY v1, Vec2.getY v2 )
                in
                (x1 * y2) - (x2 * y1)

            verticesToEdges vertices =
                case vertices of
                    first :: second :: rest ->
                        ( first, second ) :: verticesToEdges (second :: rest)

                    _ ->
                        []

            isPointInPolygon point polygon =
                let
                    normals =
                        List.map
                            (\( v1, v2 ) ->
                                let
                                    ( x1, x2 ) =
                                        ( Vec2.getX v1, Vec2.getX v2 )

                                    ( y1, y2 ) =
                                        ( Vec2.getY v1, Vec2.getY v2 )

                                    ( px, py ) =
                                        ( Vec2.getX point, Vec2.getY point )
                                in
                                crossProduct (vec2 (x2 - x1) (y2 - y1)) (vec2 (px - x1) (py - y1))
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
                    isPointInPolygon bulletVertex asteroidVertices
            )
            False
            bulletVertices

    else
        False


isCircleInsideCircle ( c1, r1 ) ( c2, r2 ) =
    let
        ( x1, x2 ) =
            ( Vec2.getX c1, Vec2.getX c2 )

        ( y1, y2 ) =
            ( Vec2.getY c1, Vec2.getY c2 )
    in
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
    -> { r | position : Vec2 }
    -> Bool
isInside { top, left, right, bottom } { position } =
    let
        { x, y } =
            Vec2.toRecord position
    in
    x > left && x < right && y > top && y < bottom
