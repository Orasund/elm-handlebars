module VerifyExamples.Handlebars.Syntax.SubExp5 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Handlebars.Syntax exposing (..)
import Result.Extra as Result
import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
import Parser







spec5 : Test.Test
spec5 =
    Test.test "#subExp: \n\n    \"test some.test\"\n    |> Parser.run subExp\n    --> Ok (Helper \"test\" (LookUp (0,[\"some\",\"test\"]),[]))" <|
        \() ->
            Expect.equal
                (
                "test some.test"
                |> Parser.run subExp
                )
                (
                Ok (Helper "test" (LookUp (0,["some","test"]),[]))
                )