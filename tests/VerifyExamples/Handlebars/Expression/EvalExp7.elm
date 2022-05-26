module VerifyExamples.Handlebars.Expression.EvalExp7 exposing (..)

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



spec7 : Test.Test
spec7 =
    Test.test "#evalExp: \n\n    evalExp Handlebars.defaultConfig\n        ( For (0,[\"people\"])\n            [(0,[ \"@index\"]) |> LookUp |> Variable]\n        )\n        value\n    --> Ok \"01\"" <|
        \() ->
            Expect.equal
                (
                evalExp Handlebars.defaultConfig
                    ( For (0,["people"])
                        [(0,[ "@index"]) |> LookUp |> Variable]
                    )
                    value
                )
                (
                Ok "01"
                )