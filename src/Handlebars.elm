module Handlebars exposing
    ( Template, Error, compile, defaultConfig, errorToString
    , eval, parse
    )

{-| compiles Handlebars templates.


# Compile Templates

@docs Template, Error, compile, defaultConfig, errorToString


# Advanced

@docs eval, parse

-}

import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import Handlebars.Expression as Expression exposing (Config, Error(..), Expression)
import Handlebars.Helper as Helper
import Handlebars.Path as Path exposing (Path, RelativePath)
import Handlebars.Syntax as Syntax
import Handlebars.Type as Type exposing (Type)
import Handlebars.Value as Value exposing (Value(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode
import Json.Value
import Parser exposing (Problem(..))
import Set exposing (Set)


{-| Template
-}
type alias Template =
    ( List Expression, Maybe Type )


{-| Possible error are either syntactical or semantical.
-}
type Error
    = SyntaxError (List Parser.DeadEnd)
    | SemanticError Expression.Error


{-| the default config.
-}
defaultConfig : Config
defaultConfig =
    { expHelpers =
        Dict.fromList
            [ ( "equals", Helper.equals )
            , ( "lookup", Helper.lookup )
            ]
    , blockHelpers =
        Dict.fromList
            [ ( "if", Helper.if_ )
            , ( "unless", Helper.unless )
            , ( "inline", Helper.inline )
            , ( "inside", Helper.inside )
            ]
    , root = []
    }


{-| parse a string into a template
-}
parse : String -> Result (List Parser.DeadEnd) Template
parse string =
    string
        |> Parser.run Syntax.parser
        |> Result.map
            (\exp ->
                ( exp
                , (case Type.ofTemplate exp of
                    Ok t ->
                        t

                    Err _ ->
                        Dict.empty
                  )
                    |> Type.normalize
                )
            )


{-| evaluate a template using a json value
-}
eval : Config -> Template -> Json.Encode.Value -> Result Expression.Error String
eval config ( expressions, _ ) v =
    let
        context =
            v
                |> Value.fromJson
    in
    expressions
        |> List.foldl
            (\exp result ->
                Expression.evalExp config exp context
                    |> Result.map2 (++) result
            )
            (Ok "")


{-| parses the template and then evaluates it.

    compile config string json =
        parse string
            |> Result.mapError SyntaxError
            |> Result.andThen
                (\expressions ->
                    eval config expressions json
                        |> Result.mapError SemanticError
                )

Tests:

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result
    import Handlebars
    import Json.Decode as D

    compile : String -> String -> (Result Error String)
    compile  template value =
        case value |> D.decodeString D.value of
            Ok v ->
                v
                    |> Handlebars.compile Handlebars.defaultConfig
                        template
            Err err -> Ok (D.errorToString err)

    "{ \"valid\":true }"
        |> compile "Hello There"
        --> (Ok "Hello There")

-}
compile : Config -> String -> Json.Encode.Value -> Result Error String
compile config string json =
    parse string
        |> Result.mapError SyntaxError
        |> Result.andThen
            (\expressions ->
                eval config expressions json
                    |> Result.mapError SemanticError
            )


{-| Print the error in a nice and readable way.
-}
errorToString : Error -> String
errorToString e0 =
    case e0 of
        SyntaxError list ->
            list
                |> deadEndsToString
                |> (++) "Syntax error: "

        SemanticError err ->
            case err of
                StringExpected ( subExp, value ) ->
                    "String expected"

                CollectionExpected path relativePath ->
                    "Either record or list expected"

                BlockHelperNotFound string ->
                    "Block Helper " ++ string ++ " was not found"

                FromBlockHelper { helper, error } ->
                    "The block Helper " ++ helper ++ " threw the following error" ++ error

                PathNotValid path relativePath ->
                    "The path is not valid"

                PathNotFound path ->
                    "The path is not found"

                HelperNotFound string ->
                    "The helper " ++ "string" ++ " was not found"

                FromHelper { helper, error } ->
                    "The Helper " ++ helper ++ " threw the following error" ++ error


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.join "\n" (List.map deadEndToString deadEnds)


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    problemToString deadEnd.problem
        ++ " at "
        ++ deadEndToRowColString deadEnd


problemToString : Parser.Problem -> String
problemToString prob =
    case prob of
        Expecting s ->
            "Expecting " ++ s

        ExpectingInt ->
            "Expecting Int"

        ExpectingHex ->
            "Expecting Hex"

        ExpectingOctal ->
            "Expecting Octal"

        ExpectingBinary ->
            "Expecting Binary"

        ExpectingFloat ->
            "Expecting Float"

        ExpectingNumber ->
            "Expecting Number"

        ExpectingVariable ->
            "Expecting Variable"

        ExpectingSymbol s ->
            "Expecting Symbol " ++ s

        ExpectingKeyword s ->
            "Expecting Keyword " ++ s

        ExpectingEnd ->
            "Expecting End"

        UnexpectedChar ->
            "Unexpected Char"

        Problem s ->
            "Problem: " ++ s

        BadRepeat ->
            "Bad Repeat"


deadEndToRowColString : Parser.DeadEnd -> String
deadEndToRowColString deadEnd =
    "row " ++ String.fromInt deadEnd.row ++ ", " ++ "col " ++ String.fromInt deadEnd.col
