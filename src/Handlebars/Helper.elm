module Handlebars.Helper exposing
    ( lookup, equals
    , if_, unless, inline, inside
    )

{-| Helpers can be set in the config type.

There are two kinds of helpers:

  - (Expression) Helpers `{{helper a b c}}`
  - (Block) Helpers `{{#helper a}} content {{/helper}}`


# Expression Helper

@docs lookup, equals


# Block Helper

@docs if_, unless, inline, inside

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Handlebars.Expression as Expression exposing (BlockHelper, ExpHelper)
import Handlebars.Path as Path exposing (Path, RelativePath)
import Handlebars.Syntax as Syntax
import Handlebars.Value as Value exposing (Value(..))
import Parser
import Result.Extra


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing

    "{ \"first\":[\"Gill\",\"Jack\"],\"second\": [\"Jack\",\"Gill\"] }"
        |> compile "{{#if (equals first second)}}equal{{/if}}"
        --> Just ""

    "{ \"first\":[\"Jack\", \"Gill\"],\"second\": [\"Jack\",\"Gill\"] }"
        |> compile "{{#if (equals first second)}}equal{{/if}}"
        --> Just "equal"

    "{ \"first\":[\"Jack\", \"Gill\"],\"second\": [\"Jack\",\"Gill\"] }"
        |> compile "{{#if (equals first.0 second.0)}}equal{{/if}}"
        --> Just "equal"

    "{ \"first\":[\"Jack\", \"Gill\"],\"second\": [\"Jack\",\"Gill\"] }"
        |> compile "{{#if (equals first.0)}}equal{{/if}}"
        --> Nothing

-}
equals : ExpHelper
equals l =
    case l of
        [ a, b ] ->
            a == b |> BooleanValue |> Ok

        _ ->
            "Needs 2 arguments, but got " ++ (l |> List.length |> String.fromInt) |> Err


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing

    "{ \"key\":\"1\",\"users\": [\"Jack\",\"Gill\"] }"
        |> compile "Hello {{lookup users key}}"
        --> Just "Hello Gill"

-}
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


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing

    "{ \"test\":\"true\" }"
        |> compile "{{#inline .}}Hello\n There{{/inline}}"
        --> Just "Hello There"

-}
inline : BlockHelper
inline { arg, content } path =
    path
        |> content
        |> Result.map
            (\string ->
                string
                    |> String.replace "\n" ""
                    |> String.replace "\u{000D}" ""
            )


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing

    "{ \"valid\":null }"
        |> compile "{{#if valid}}Hello There{{/if}}"

        --> Just ""

    "{ \"valid\":false }"
        |> compile "{{#if valid}}Hello There{{/if}}"

        --> Just ""

    "{ \"valid\":true }"
        |> compile "{{#if valid}}Hello There{{/if}}"

        --> Just "Hello There"

    "{ \"name\":\"Jack\" }"
        |> compile "{{#if name}}Hello There{{/if}}"

        --> Just "Hello There"

    "{ \"name\":\"\" }"
        |> compile "{{#if name}}Hello There{{/if}}"

        --> Just ""

    "{ \"list\": [] }"
        |> compile "{{#if list}}Hello There{{/if}}"

        --> Just ""

    "{ \"list\": [ \"something\" ] }"
        |> compile "{{#if list}}Hello There{{/if}}"

        --> Just "Hello There"

    "{ \"list\": [ \"something\" ] }"
        |> compile "{{#if .}}Hello There{{/if}}"

        --> Just "Hello There"

    "{}"
        |> compile "{{#if .}}Hello There{{/if}}"

        --> Just ""

-}
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


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing

    "{ \"valid\":null }"
        |> compile "{{#unless valid}}Hello There{{/unless}}"

        --> Just "Hello There"

    "{ \"valid\":false }"
        |> compile "{{#unless valid}}Hello There{{/unless}}"

        --> Just "Hello There"

    "{ \"valid\":true }"
        |> compile "{{#unless valid}}Hello There{{/unless}}"

        --> Just ""

    "{ \"name\":\"Jack\" }"
        |> compile "{{#unless name}}Hello There{{/unless}}"

        --> Just ""

    "{ \"name\":\"\" }"
        |> compile "{{#unless name}}Hello There{{/unless}}"

        --> Just "Hello There"

    "{ \"list\": [] }"
        |> compile "{{#unless list}}Hello There{{/unless}}"

        --> Just "Hello There"

    "{ \"list\": [ \"something\" ] }"
        |> compile "{{#unless list}}Hello There{{/unless}}"

        --> Just ""

    "{ \"list\": [ \"something\" ] }"
        |> compile "{{#unless .}}Hello There{{/unless}}"

        --> Just ""

    "{}"
        |> compile "{{#unless .}}Hello There{{/unless}}"

        --> Just "Hello There"

-}
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


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Handlebars exposing (Error(..))
    import Result.Extra as Result
    import Json.Decode as D

    compile : String -> String -> Maybe String
    compile template value =
        case value |> D.decodeString D.value of
            Ok v ->
                case
                    v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
                of
                Ok result -> Just result
                Err _ -> Nothing
            Err _ -> Nothing


    "{ \"name\":\"Jack\",\"key\":\"name\" }"
        |> compile "{{#inside key}}{{.}}{{/inside}}"

        --> Just ("Jack")

    "{ \"name\":\"Jack\",\"key\":false }"
        |> compile "{{#inside key}}{{.}}{{/inside}}"

        --> Nothing

    "{ \"name\":\"Jack\",\"key\":0 }"
        |> compile "{{#inside key}}{{.}}{{/inside}}"

        --> Nothing
    "{ \"name\":\"Jack\",\"key\":\"notAPath...\" }"
        |> compile "{{#inside key}}{{.}}{{/inside}}"

        --> Nothing

-}
inside : BlockHelper
inside { arg, throw, content } path =
    case arg of
        StringValue string ->
            string
                |> Parser.run Syntax.path
                |> Result.mapError (\_ -> "path expected but got " ++ string |> throw)
                |> Result.andThen
                    (\relativePath ->
                        path
                            |> Path.withRelativePath relativePath
                            |> Maybe.map content
                            |> Maybe.withDefault (throw "path is invalid" |> Err)
                    )

        _ ->
            throw "expected a string" |> Err
