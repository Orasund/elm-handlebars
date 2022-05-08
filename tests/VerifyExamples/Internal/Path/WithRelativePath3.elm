module VerifyExamples.Internal.Path.WithRelativePath3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Internal.Path exposing (..)







spec3 : Test.Test
spec3 =
    Test.test "#withRelativePath: \n\n    [ \"people\", \"jack\"]\n        |> withRelativePath []\n    --> Just [\"people\",\"jack\"]" <|
        \() ->
            Expect.equal
                (
                [ "people", "jack"]
                    |> withRelativePath []
                )
                (
                Just ["people","jack"]
                )