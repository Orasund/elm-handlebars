module Handlebars exposing (..)

import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import Handlebars.Expression as Expression exposing (Config, Error, Expression)
import Handlebars.Helper as Helper
import Handlebars.Value as Value exposing (Value(..))
import Internal.Path as Path exposing (Path, RelativePath)
import Json.Decode as D exposing (Decoder)
import Json.Encode
import Json.Value
import Parser
import Set exposing (Set)


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
            ]
    , root = []
    }


eval : Config -> List Expression -> Json.Encode.Value -> Result Error String
eval config expressions v =
    let
        context =
            v
                |> Json.Value.decodeValue
                |> Value.fromJson
    in
    expressions
        |> List.foldl
            (\exp result ->
                Expression.evalExp config exp context
                    |> Result.map2 (++) result
            )
            (Ok "")
