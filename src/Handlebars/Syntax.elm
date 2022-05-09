module Handlebars.Syntax exposing (..)

import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
import Handlebars.Value exposing (Value(..))
import Internal.Path as Path exposing (RelativePath)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Set


{-|

    import Parser
    import Handlebars.Expression as Expression exposing (Expression(..), SubExp(..))
    import Result.Extra as Result

    "Hello {{"
    |> Parser.run exp
    --> Ok (Text "Hello ")

    "{{test}}"
    |> Parser.run exp
    --> Ok (Variable (LookUp (0,["test"])))

-}
exp : Parser Expression
exp =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol "{{"
            |= Parser.oneOf
                [ Parser.succeed (\p1 e p2 -> ( p1, p2, For p1 e ))
                    |. Parser.symbol "#"
                    |= path
                    |. Parser.symbol "}}"
                    |= Parser.lazy (\() -> exp)
                    |. Parser.symbol "{{/"
                    |= path
                    |> Parser.andThen
                        (\( p1, p2, e ) ->
                            if p1 == p2 then
                                Parser.succeed e

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
        , Parser.chompUntilEndOr "{{"
            |> Parser.getChompedString
            |> Parser.map Text
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


variable : Parser String
variable =
    let
        list =
            [ '#', '}', '{', ' ', '\n', '\u{000D}', '.', '(', ')' ]
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
    let
        rec =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol "."
                    |= variable
                , variable
                ]
    in
    Parser.succeed Tuple.pair
        |= (internalRepeat (Parser.symbol "../") |> Parser.map List.length)
        |= Parser.oneOf
            [ Parser.succeed []
                |. Parser.symbol "."
            , internalRepeat rec
            ]


internalRepeat : Parser a -> Parser (List a)
internalRepeat parser =
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
    Parser.loop [] (manyHelp parser)
