module VerifyExamples.Handlebars.Value.Get1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Handlebars.Value exposing (..)
import Array
import Dict



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



spec1 : Test.Test
spec1 =
    Test.test "#get: \n\n    Array.empty\n        |> ArrayValue\n        |> get [\"0\",\"@first\"]\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                Array.empty
                    |> ArrayValue
                    |> get ["0","@first"]
                )
                (
                Nothing
                )