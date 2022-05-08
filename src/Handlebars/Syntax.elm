module Handlebars.Syntax exposing (..)

import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
import Handlebars.Value exposing (Value(..))
import Internal.Path exposing (RelativePath)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Set


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))

    "Hello {{"
    |> Parser.run exp
    --> Ok (Text "Hello ")

    "{{test}}"
    |> Parser.run exp
    --> Ok (Variable (LookUp [Just "test"]))

    "{{test a b c}}"
    |> Parser.run exp
    --> Ok (Variable (Helper "test" (LookUp [Just "a"],[LookUp [Just "b"],LookUp [Just "c"]])))

-}
exp : Parser Expression
exp =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "{{"
            |= Parser.oneOf
                [ Parser.succeed
                    (\name args ->
                        Variable
                            (case args of
                                [] ->
                                    LookUp [ Just name ]

                                head :: tail ->
                                    Helper name
                                        ( LookUp [ Just head ]
                                        , tail |> List.map (\it -> LookUp [ Just it ])
                                        )
                            )
                    )
                    |= variable
                    |. Parser.chompWhile ((==) ' ')
                    |= Parser.Extras.many variable
                ]
            |. Parser.symbol "}}"
        , Parser.chompUntilEndOr "{{"
            |> Parser.getChompedString
            |> Parser.map Text
        ]


variable : Parser String
variable =
    Parser.variable
        { start = \c -> [ '#', '}', '{', ' ', '\n', '\u{000D}', '.' ] |> List.member c |> not
        , inner = \c -> [ '#', '}', '{', ' ', '\n', '\u{000D}', '.' ] |> List.member c |> not
        , reserved = Set.empty
        }


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))

    ""
    |> Parser.run path
    |> (\err ->
        case err of
            Err _ ->
                False
            Ok _ ->
                True
        )
    --> False

    "."
    |> Parser.run path
    --> Ok []

-}
path : Parser RelativePath
path =
    let
        rec =
            Parser.oneOf
                [ Parser.succeed Nothing
                    |. Parser.symbol "../"
                , Parser.succeed Just
                    |. Parser.symbol "."
                    |= variable
                , Parser.succeed Just
                    |= variable
                ]
    in
    Parser.oneOf
        [ Parser.succeed (\b -> Nothing :: b)
            |. Parser.symbol "../"
            |= Parser.Extras.many rec
        , Parser.succeed []
            |. Parser.symbol "."
        , Parser.succeed (\a b -> Just a :: b)
            |= variable
            |= Parser.Extras.many rec
        ]
