module VerifyExamples.Handlebars.Expression.EvalSubExp0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Handlebars.Expression exposing (..)
import Handlebars.Value exposing (Value(..))
import Handlebars exposing (defaultConfig)
import Dict



value : Value
value =
    [ ( "name", jack )
    , ( "key", StringValue "name" )
    ]
        |> Dict.fromList
        |> ObjectValue
jack : Value
jack =
    StringValue "jack"



spec0 : Test.Test
spec0 =
    Test.test "#evalSubExp: \n\n    Helper \"lookup\" (LookUp [],[])\n        |> evalSubExp defaultConfig value\n        |> (\\err ->\n            case err of\n                Err (FromHelper args) ->\n                    args.helper == \"lookup\"\n                _ ->\n                    False\n        )\n    --> True" <|
        \() ->
            Expect.equal
                (
                Helper "lookup" (LookUp [],[])
                    |> evalSubExp defaultConfig value
                    |> (\err ->
                        case err of
                            Err (FromHelper args) ->
                                args.helper == "lookup"
                            _ ->
                                False
                    )
                )
                (
                True
                )