module VerifyExamples.Internal.Path.RelativeToString6 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Expect
import Handlebars.Path exposing (..)
import Test


spec6 : Test.Test
spec6 =
    Test.test "#relativeToString: \n\n    (1,[])\n        |> relativeToString\n    --> \"../.\"" <|
        \() ->
            Expect.equal
                (( 1, [] )
                    |> relativeToString
                )
                "../."
