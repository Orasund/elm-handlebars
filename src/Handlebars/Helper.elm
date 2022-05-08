module Handlebars.Helper exposing (..)

import Array exposing (Array)
import Dict
import Handlebars.Expression exposing (BlockHelper, ExpHelper)
import Handlebars.Value as Value exposing (Value(..))
import Result.Extra


equals : ExpHelper
equals l =
    case l of
        [ a, b ] ->
            a == b |> BooleanValue |> Ok

        _ ->
            "Needs 2 arguments, but got " ++ (l |> List.length |> String.fromInt) |> Err


lookup : ExpHelper
lookup l =
    case l of
        [ ArrayValue array, StringValue string ] ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)
                |> Maybe.map Ok
                |> Maybe.withDefault ("index " ++ string ++ " no found." |> Err)

        [ ObjectValue dict, StringValue string ] ->
            dict
                |> Dict.get string
                |> Maybe.map Ok
                |> Maybe.withDefault ("key " ++ string ++ " not found." |> Err)

        [ _, _ ] ->
            "First argument needs to be an array or an object." |> Err

        _ ->
            "Needs 2 arguments, but got " ++ (l |> List.length |> String.fromInt) |> Err


if_ : BlockHelper
if_ { arg, content } =
    if
        case arg of
            BooleanValue bool ->
                bool /= False

            StringValue string ->
                string /= ""

            ArrayValue array ->
                array /= Array.empty

            ObjectValue dict ->
                dict /= Dict.empty
    then
        content

    else
        \_ -> Ok ""


unless : BlockHelper
unless { arg, content } =
    if
        case arg of
            BooleanValue bool ->
                bool == False

            StringValue string ->
                string == ""

            ArrayValue array ->
                array == Array.empty

            ObjectValue dict ->
                dict == Dict.empty
    then
        content

    else
        \_ -> Ok ""


each : BlockHelper
each { arg, content, throw } args =
    case arg of
        ArrayValue array ->
            array
                |> Array.toList
                |> List.indexedMap
                    (\i _ ->
                        content
                            { path = args.path ++ [ String.fromInt i ]
                            , context = args.context
                            }
                    )
                |> Result.Extra.combine
                |> Result.map String.concat

        ObjectValue dict ->
            dict
                |> Dict.keys
                |> List.map (\key -> content { args | path = args.path ++ [ key ] })
                |> Result.Extra.combine
                |> Result.map String.concat

        _ ->
            "argument must be either an array or an object"
                |> throw
                |> Err
