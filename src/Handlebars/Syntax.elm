module Handlebars.Syntax exposing (exp, parser, path, subExp, variable)

{-| Parsing a Template

@docs exp, parser, path, subExp, variable

-}

import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
import Handlebars.Path as Path exposing (RelativePath)
import Handlebars.Value exposing (Value(..))
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Set


{-| Parser for a template
-}
parser : Parser (List Expression)
parser =
    internalRepeat exp


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result

text

    "Hello {{"
    |> Parser.run exp
    --> Ok (Text "Hello ")

    "Hello \\{\\{world\\}\\}"
    |> Parser.run exp
    --> Ok (Text "Hello {{world}}")

variables

    "{{test}}"
    |> Parser.run exp
    --> Ok (Variable (LookUp (0,["test"])))

For block

    "{{#test}}hello world{{/test}}"
        |> Parser.run exp
    --> Ok (For (0,["test"]) [Text "hello world"])

    "{{#some}}hello world{{/test}}"
        |> Parser.run exp
        |> Result.isOk
    --> False

    "{{#test}}hello world"
        |> Parser.run exp
        |> Result.isOk
    --> False

Helper Block

    "{{#test a}}hello world{{/test}}"
    |> Parser.run exp
    --> Ok (Block "test" (LookUp (0,["a"])) [Text "hello world"])

    "{{#some.test a}}hello world{{/test}}"
    |> Parser.run exp
    |> Result.isOk
    --> False

    "{{#blockHelper (helper a)}}hello world{{/blockHelper}}"
    |> Parser.run exp
    --> Ok (Block "blockHelper" (Helper "helper" (LookUp (0,["a"]),[])) [Text "hello world"])

    "{{#test}}hello {{name}}{{/test}}"
    |> Parser.run exp
    --> Ok (For (0,["test"]) [Text "hello ",Variable (LookUp (0,["name"]))])

    "{{#test}}{{#test}}hello world{{/test}}{{/test}}"
    |> Parser.run exp
    --> Ok (For (0,["test"]) [For (0,["test"]) [Text "hello world"]])

-}
exp : Parser Expression
exp =
    Parser.oneOf
        [ Parser.succeed (++)
            |= Parser.variable
                { start = (/=) '{'
                , inner = \_ -> False
                , reserved = Set.empty
                }
            |= (Parser.chompUntilEndOr "{{"
                    |> Parser.getChompedString
               )
            |> Parser.map
                (\string ->
                    string
                        |> String.replace "\\\\" "\\"
                        |> String.replace "\\{" "{"
                        |> String.replace "\\}" "}"
                        |> Text
                )
        , Parser.succeed identity
            |. Parser.symbol "{{"
            |= Parser.oneOf
                [ Parser.succeed (\p1 arg e1 e2 p2 -> { p1 = p1, p2 = p2, arg = arg, e = e1 :: e2 })
                    |. Parser.symbol "#"
                    |= path
                    |= Parser.oneOf
                        [ Parser.succeed Just
                            |. Parser.chompWhile ((==) ' ')
                            |= Parser.oneOf
                                [ Parser.succeed identity
                                    |. Parser.symbol "("
                                    |= Parser.lazy (\() -> subExp)
                                    |. Parser.symbol ")"
                                , Parser.lazy (\() -> subExp)
                                ]
                        , Parser.succeed Nothing
                        ]
                    |. Parser.symbol "}}"
                    |= Parser.lazy (\() -> exp)
                    |= internalRepeat (Parser.lazy (\() -> exp))
                    |. Parser.symbol "{{/"
                    |= path
                    |> Parser.andThen
                        (\{ p1, p2, arg, e } ->
                            if p1 == p2 then
                                case ( p1, arg ) of
                                    ( _, Nothing ) ->
                                        For p1 e |> Parser.succeed

                                    ( ( 0, [ name ] ), Just a ) ->
                                        Block name a e |> Parser.succeed

                                    ( _, Just a ) ->
                                        "The block "
                                            ++ Path.relativeToString p1
                                            ++ " has an argument, but its not a helper."
                                            |> Parser.problem

                            else
                                "The block starts with "
                                    ++ Path.relativeToString p1
                                    ++ ", but ends with "
                                    ++ Path.relativeToString p2
                                    |> Parser.problem
                        )
                , subExp |> Parser.map Variable
                ]
            |. Parser.symbol "}}"
            |> Parser.backtrackable
        ]


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result

    "test some.test"
    |> Parser.run subExp
    --> Ok (Helper "test" (LookUp (0,["some","test"]),[]))

    "test a b c"
    |> Parser.run subExp
    --> Ok (Helper "test" (LookUp (0,["a"]),[LookUp (0,["b"]),LookUp (0,["c"])]))

    "some.test a"
    |> Parser.run subExp
    |> Result.isOk
    --> False

    "some (test a) b"
    |> Parser.run subExp
    --> Ok (Helper "some" ( (Helper "test" (LookUp (0,["a"]),[]) ), [LookUp (0,["b"])]) )

    "(test)"
    |> Parser.run subExp
    |> Result.isOk
    --> False

    "some (a)"
    |> Parser.run subExp
    --> Ok (Helper "some" (LookUp (0,["a"]),[]))

-}
subExp : Parser SubExp
subExp =
    (Parser.succeed Tuple.pair
        |= path
        |. Parser.chompWhile ((==) ' ')
        |= Parser.Extras.many
            (Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol "("
                    |= Parser.lazy (\() -> subExp)
                    |. Parser.symbol ")"
                , path |> Parser.map LookUp
                ]
            )
    )
        |> Parser.andThen
            (\( relativePath, args ) ->
                case ( relativePath, args ) of
                    ( r, [] ) ->
                        LookUp r |> Parser.succeed

                    ( ( 0, [ name ] ), head :: tail ) ->
                        Helper name
                            ( head
                            , tail
                            )
                            |> Parser.succeed

                    ( _, head :: tail ) ->
                        "'.' is not allowed in variable name" |> Parser.problem
            )


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result

    ""
        |> Parser.run variable
        |> Result.isOk
        --> False

-}
variable : Parser String
variable =
    let
        list =
            [ '#', '}', '{', ' ', '\n', '\u{000D}', '.', '(', ')', '/' ]
    in
    Parser.variable
        { start = \c -> list |> List.member c |> not
        , inner = \c -> list |> List.member c |> not
        , reserved = Set.empty
        }


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result

    ""
    |> Parser.run path
    |> Result.isOk
    --> False

    "."
    |> Parser.run path
    --> Ok (0,[])

    "../"
    |> Parser.run path
    |> Result.isOk
    --> False

    "../."
    |> Parser.run path
    --> Ok (1,[])

    "some.test"
    |> Parser.run path
    --> Ok (0,["some", "test"])

    "../some"
    |> Parser.run path
    --> Ok (1,["some"])

    "some.test a"
    |> Parser.run path
    --> Ok (0,["some", "test"])

-}
path : Parser RelativePath
path =
    Parser.succeed Tuple.pair
        |= (internalRepeat (Parser.symbol "../") |> Parser.map List.length)
        |= Parser.oneOf
            [ Parser.succeed []
                |. Parser.symbol "."
            , Parser.succeed (::)
                |= variable
                |= internalRepeat
                    (Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.symbol "."
                            |= variable
                        , variable
                        ]
                    )
            ]


internalRepeat : Parser a -> Parser (List a)
internalRepeat par =
    let
        manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
        manyHelp p vs =
            Parser.oneOf
                [ Parser.succeed (\v -> Parser.Loop (v :: vs))
                    |= p
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse vs))
                ]
    in
    Parser.loop [] (manyHelp par)
