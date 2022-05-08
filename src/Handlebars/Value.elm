module Handlebars.Value exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.Path exposing (Path)


type Value
    = StringValue String
    | BooleanValue Bool
    | ArrayValue (Array Value)
    | ObjectValue (Dict String Value)


{-| Get a value by the path.

Use `@key` to get the key of a value.

    import Dict

    object : Value
    object = Dict.fromList [ ( "name", StringValue "Jack" ) ] |> ObjectValue

    object
        |> get [ "name" ]
        --> Just (StringValue "Jack")

    object
        |> get [ "name", "@key" ]
        --> Just (StringValue "name")

    [ ( "name"
      , [ ( "@key", StringValue "impossible State" ) ] |> Dict.fromList |> ObjectValue
      )
    ]
        |> Dict.fromList
        |> ObjectValue
        |> get [ "name", "@key" ]
        --> Just (StringValue "name")

    object
        |> get [ "@key" ]
        --> Nothing

    object
        |> get []
        --> Just object

Use `@index` to get the index

    import Array

    jack : Value
    jack = StringValue "Jack"

    gill : Value
    gill = StringValue "Gill"

    array : Value
    array =
        [ jack, gill ]
        |> Array.fromList
        |> ArrayValue

    array
        |> get []
        --> Just array

    array
        |> get ["0"]
        --> Just jack

    array
        |> get ["0","@index"]
        --> Just (StringValue "0")

    array
        |> get ["0","@first"]
        --> Just (BooleanValue True)

    array
        |> get ["0","@last"]
        --> Just (BooleanValue False)

    array
        |> get ["1"]
        --> Just gill

    array
        |> get ["1","@index"]
        --> Just (StringValue "1")

    array
        |> get ["1","@last"]
        --> Just (BooleanValue True)

    array
        |> get ["0","@last"]
        --> Just (BooleanValue False)

    array
        |> get ["-1"]
        --> Nothing

    Array.empty
        |> ArrayValue
        |> get ["0"]
        --> Nothing

    Array.empty
        |> ArrayValue
        |> get ["0","@index"]
        --> Nothing

    Array.empty
        |> ArrayValue
        |> get ["0","@first"]
        --> Nothing

    Array.empty
        |> ArrayValue
        |> get ["0","@last"]
        --> Nothing

For Strings and Boolean only an empty path is allowed.

    jack
        |> get ["something"]
        |> Nothing

    jack
        |> get []
        |> Just jack

    isValid : Value
    isValid =
        BooleanValue True

    isValid
        |> get ["something"]
        |> Nothing

    isValid
        |> get []
        |> Just isValid

-}
get : Path -> Value -> Maybe Value
get path value =
    case ( value, path ) of
        ( ObjectValue dict, [ string, "@key" ] ) ->
            dict
                |> Dict.get string
                |> Maybe.map (\_ -> StringValue string)

        ( ObjectValue dict, head :: tail ) ->
            dict
                |> Dict.get head
                |> Maybe.andThen (get tail)

        ( ArrayValue array, [ string, "@index" ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)
                |> Maybe.map (\_ -> StringValue string)

        ( ArrayValue array, [ string, "@first" ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)
                |> Maybe.map (\_ -> string == "0" |> BooleanValue)

        ( ArrayValue array, [ string, "@last" ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)
                |> Maybe.map
                    (\_ ->
                        array
                            |> Array.length
                            |> (+) -1
                            |> String.fromInt
                            |> (==) string
                            |> BooleanValue
                    )

        ( ArrayValue array, [ string ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)

        ( _, _ :: _ ) ->
            Nothing

        ( _, [] ) ->
            Just value
