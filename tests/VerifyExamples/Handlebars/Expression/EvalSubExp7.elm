module VerifyExamples.Handlebars.Expression.EvalSubExp7 exposing (..)

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



spec7 : Test.Test
spec7 =
    Test.test "#evalSubExp: \n\n    LookUp (0,[\"name\"])\n        |> evalSubExp defaultConfig value\n    --> Ok jack" <|
        \() ->
            Expect.equal
                (
                LookUp (0,["name"])
                    |> evalSubExp defaultConfig value
                )
                (
                Ok jack
                )