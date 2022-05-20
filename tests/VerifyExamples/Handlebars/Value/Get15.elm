module VerifyExamples.Handlebars.Value.Get15 exposing (..)

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



spec15 : Test.Test
spec15 =
    Test.test "#get: \n\n    array\n        |> get [\"0\",\"@index\"]\n    --> Just (StringValue \"0\")" <|
        \() ->
            Expect.equal
                (
                array
                    |> get ["0","@index"]
                )
                (
                Just (StringValue "0")
                )