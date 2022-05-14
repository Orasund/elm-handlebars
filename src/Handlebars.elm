module Handlebars exposing
    ( Template, Error, compile, defaultConfig
    , eval, parse
    )

{-| compiles Handlebars templates.


# Compile Templates

@docs Template, Error, compile, defaultConfig


# Advanced

@docs eval, parse

-}

import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import Handlebars.Expression as Expression exposing (Config, Expression)
import Handlebars.Helper as Helper
import Handlebars.Path as Path exposing (Path, RelativePath)
import Handlebars.Syntax as Syntax
import Handlebars.Value as Value exposing (Value(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode
import Json.Value
import Parser
import Set exposing (Set)


{-| Template
-}
type alias Template =
    List Expression


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
parse =
    Parser.run Syntax.parser


{-| evaluate a template using a json value
-}
eval : Config -> Template -> Json.Encode.Value -> Result Expression.Error String
eval config expressions v =
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
