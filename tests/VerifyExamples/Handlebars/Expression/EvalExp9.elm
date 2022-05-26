module VerifyExamples.Handlebars.Expression.EvalExp9 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Handlebars.Expression exposing (..)
import Array
import Handlebars
import Handlebars.Value exposing (Value(..))
import Dict



value : Value
value =
    [ ( "name", jack )
    , ( "key", StringValue "name" )
    , ( "valid", BooleanValue True)
    , ( "people",
        [ StringValue "jack" , StringValue "gill" ]
        |> Array.fromList
        |> ArrayValue
    )
    ]
        |> Dict.fromList
        |> ObjectValue
jack : Value
jack =
    StringValue "jack"



spec9 : Test.Test
spec9 =
    Test.test "#evalExp: \n\n    evalExp Handlebars.defaultConfig\n        ( (0,[\"name\"])\n            |> LookUp\n            |> Variable\n        )\n        value\n    --> Ok \"jack\"" <|
        \() ->
            Expect.equal
                (
                evalExp Handlebars.defaultConfig
                    ( (0,["name"])
                        |> LookUp
                        |> Variable
                    )
                    value
                )
                (
                Ok "jack"
                )