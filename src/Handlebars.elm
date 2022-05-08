module Handlebars exposing (..)

import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import Handlebars.Expression exposing (Config)
import Handlebars.Helper as Helper
import Handlebars.Value as Value exposing (Value(..))
import Internal.Path as Path exposing (Path, RelativePath)
import Json.Decode as D exposing (Decoder)
import Parser
import Set exposing (Set)


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
