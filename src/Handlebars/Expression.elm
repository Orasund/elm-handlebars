module Handlebars.Expression exposing (..)

import Dict exposing (Dict)
import Handlebars.Value as Value exposing (Value(..))
import Internal.Path as Path exposing (Path, RelativePath)
import Result.Extra


type alias Config =
    { expHelpers : Dict String ExpHelper
    , blockHelpers : Dict String BlockHelper
    , root : Path
    }


type alias ExpHelper =
    List Value -> Result String Value


type alias BlockHelper =
    { arg : Value
    , throw : String -> Error
    , content : { path : Path, context : Value } -> Result Error String
    }
    -> { path : Path, context : Value }
    -> Result Error String


type SubExp
    = LookUp RelativePath --some.path
    | Helper String ( SubExp, List SubExp ) --helper a b c


type Expression
    = Text String
    | Variable SubExp --{{subExp}}
    | Block String SubExp Expression --{{#name subExp }} exp {{/name}}


type Error
    = StringExpected ( SubExp, Value )
    | BlockHelperNot String
    | FromBlockHelper { helper : String, error : String }
    | AtSubExp SubExpError


type SubExpError
    = PathNotValid Path RelativePath
    | PathNotFound Path
    | HelperNotFound String
    | FromHelper { helper : String, error : String }


{-| Evaluate a subexpression

    import Dict
    import Handlebars.Value exposing (Value(..))

    expression : Expression
    expression =
        Text ""

    jack : Value
    jack =
        StringValue "jack"

    value : Value
    value =
        [ ( "name", jack )
        , ( "key", StringValue "name" )
        ]
            |> Dict.fromList
            |> ObjectValue

Simplest subexpression is a look up to a relative path.

    LookUp [Just "name"]
        |> evalSubExp defaultConfig value
        --> Ok jack

    LookUp [Just "job"]
        |> evalSubExp defaultConfig  value
        --> Err (PathNotFound ["job"])

    LookUp [Nothing]
        |> evalSubExp defaultConfig value
        --> Err (PathNotValid [] [Nothing])

    LookUp [Nothing]
        |> evalSubExp {defaultConfig | root = ["name"]} value
        --> Ok value

Helper can also be used inside of subexpression.

    Helper "lookup" ( LookUp [], [LookUp [ Just "key" ]] )
        |> evalSubExp defaultConfig value
        --> Ok jack

    Helper "lookup" ( LookUp [ Just "name"], [LookUp [ Just "key" ]] )
        |> evalSubExp defaultConfig value
        |> (\err ->
            case err of
                Err (FromHelper args) ->
                    args.helper == "lookup"
                _ ->
                    False
        )
        --> True

    Helper "lookup" (LookUp [],[])
        |> evalSubExp defaultConfig value
        |> (\err ->
            case err of
                Err (FromHelper args) ->
                    args.helper == "lookup"
                _ ->
                    False
        )
        --> True

-}
evalSubExp : Config -> Value -> SubExp -> Result SubExpError Value
evalSubExp template value e1 =
    case e1 of
        LookUp relativePath ->
            template.root
                |> Path.withRelativePath relativePath
                |> Maybe.map
                    (\path ->
                        value
                            |> Value.get path
                            |> Maybe.map Ok
                            |> Maybe.withDefault (path |> PathNotFound |> Err)
                    )
                |> Maybe.withDefault (PathNotValid template.root relativePath |> Err)

        Helper name ( head, tail ) ->
            case template.expHelpers |> Dict.get name of
                Just fun ->
                    head
                        :: tail
                        |> List.map (evalSubExp template value)
                        |> Result.Extra.combine
                        |> Result.andThen
                            (\list ->
                                list
                                    |> fun
                                    |> Result.mapError
                                        (\error ->
                                            FromHelper { helper = name, error = error }
                                        )
                            )

                Nothing ->
                    name |> HelperNotFound |> Err


{-| Evaluate a value based on a template

    import Dict
    import Handlebars.Value exposing (Value(..))

    jack : Value
    jack =
        StringValue "jack"

    value : Value
    value =
        [ ( "name", jack )
        , ( "key", StringValue "name" )
        , ( "isValid", BooleanValue True)
        ]
            |> Dict.fromList
            |> ObjectValue

    evalExp defaultConfig
        ("Hello World"
            |> Text
        )
        value
        --> Ok "Hello World"

    evalExp defaultConfig
        ([ Just "name"]
            |> LookUp
            |> Variable
        )
        value
        --> Ok "jack"

    evalExp defaultConfig
        ( Helper "equals"
            ( LookUp [ Just "name"]
            , [ LookUp [ Just "key"] ]
            )
            |> Variable
        )
        value
        |> (\err ->
            case err of
                Err (StringExpected _) ->
                    True
                _ ->
                    False
            )
        --> True

    evalExp defaultConfig
        ( Block "if" (LookUp [Just "isValid"])
            (Text "Hello")
        )
        value
        --> Ok "Hello"

    evalExp defaultConfig
        ( Block "invalid" (LookUp [])
            (Text "Hello")
        )
        value
        --> Err (BlockHelperNot "invalid")

-}
evalExp : Config -> Expression -> Value -> Result Error String
evalExp config expression value =
    case expression of
        Text string ->
            Ok string

        Variable subExp ->
            subExp
                |> evalSubExp config value
                |> Result.mapError AtSubExp
                |> Result.andThen
                    (\v ->
                        case v of
                            StringValue string ->
                                Ok string

                            _ ->
                                Err (StringExpected ( subExp, v ))
                    )

        Block string subExp exp ->
            case config.blockHelpers |> Dict.get string of
                Just fun ->
                    subExp
                        |> evalSubExp config value
                        |> Result.mapError AtSubExp
                        |> Result.andThen
                            (\arg ->
                                fun
                                    { arg = arg
                                    , throw = \err -> FromBlockHelper { helper = string, error = err }
                                    , content =
                                        \args ->
                                            args.context
                                                |> evalExp { config | root = args.path } exp
                                    }
                                    { path = config.root
                                    , context = value
                                    }
                            )

                Nothing ->
                    BlockHelperNot string |> Err
