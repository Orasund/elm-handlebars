module VerifyExamples.Handlebars.Value.Get21 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Handlebars.Value exposing (..)
import Array
import Dict



isValid : Value
isValid =
    BooleanValue True
array : Value
array =
    [ jack, gill ]
    |> Array.fromList
    |> ArrayValue
gill : Value
gill = StringValue "Gill"
jack : Value
jack = StringValue "Jack"
object : Value
object = Dict.fromList [ ( "name", StringValue "Jack" ) ] |> ObjectValue



spec21 : Test.Test
spec21 =
    Test.test "#get: \n\n    object\n        |> get [ \"name\", \"@key\" ]\n    --> Just (StringValue \"name\")" <|
        \() ->
            Expect.equal
                (
                object
                    |> get [ "name", "@key" ]
                )
                (
                Just (StringValue "name")
                )