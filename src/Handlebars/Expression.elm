module Handlebars.Expression exposing (BlockHelper, Config, Error(..), ExpHelper, Expression(..), SubExp(..), evalExp, evalSubExp)

{-| A Handlebar template is composed out of expression.

@docs BlockHelper, Config, Error, ExpHelper, Expression, SubExp, evalExp, evalSubExp

-}

import Array
import Dict exposing (Dict)
import Handlebars.Path as Path exposing (Path, RelativePath)
import Handlebars.Value as Value exposing (Value(..))
import Result.Extra


{-| The config can be used to extend the helpers.
-}
type alias Config =
    { expHelpers : Dict String ExpHelper
    , blockHelpers : Dict String BlockHelper
    , root : Path
    }


{-| Expression Helper
-}
type alias ExpHelper =
    List Value -> Result String Value


{-| Block Helper
-}
type alias BlockHelper =
    { arg : Value
    , throw : String -> Error
    , content : Path -> Result Error String
    }
    -> Path
    -> Result Error String


{-| Sub expressions
-}
type SubExp
    = LookUp RelativePath --some.path
    | Helper String ( SubExp, List SubExp ) --helper a b c


{-| Expression
-}
type Expression
    = Text String
    | Variable SubExp --{{subExp}}
    | For RelativePath (List Expression) --{{#path}} content {{/path}}
    | Block String SubExp (List Expression) --{{#name subExp }} exp {{/name}}


{-| Evaluation Errors
-}
type Error
    = StringExpected ( SubExp, Value )
    | CollectionExpected Path RelativePath
    | BlockHelperNotFound String
    | FromBlockHelper { helper : String, error : String }
    | PathNotValid Path RelativePath
    | PathNotFound Path
    | HelperNotFound String
    | FromHelper { helper : String, error : String }


{-| Evaluate a subexpression

    import Dict
    import Handlebars exposing (defaultConfig)
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

    LookUp (0,["name"])
        |> evalSubExp defaultConfig value
        --> Ok jack

    LookUp (0,["job"])
        |> evalSubExp defaultConfig  value
        --> Err (PathNotFound ["job"])

    LookUp (1,[])
        |> evalSubExp defaultConfig value
        --> Err (PathNotValid [] (1,[]))

    LookUp (1,[])
        |> evalSubExp {defaultConfig | root = ["name"]} value
        --> Ok value

Helper can also be used inside of subexpression.

    Helper "lookup" ( LookUp (0,[]), [LookUp (0,[ "key" ])] )
        |> evalSubExp defaultConfig value
        --> Ok jack

    Helper "lookup" ( LookUp (0,[ "name"]), [LookUp (0,[ "key" ])] )
        |> evalSubExp defaultConfig value
        |> (\err ->
            case err of
                Err (FromHelper args) ->
                    args.helper == "lookup"
                _ ->
                    False
        )
        --> True

    Helper "lookup" (LookUp (0,[]),[])
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
evalSubExp : Config -> Value -> SubExp -> Result Error Value
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
    import Handlebars
    import Array

    jack : Value
    jack =
        StringValue "jack"

    value : Value
    value =
        [ ( "name", jack )
        , ( "key", StringValue "name" )
        , ( "valid", BooleanValue True)
        , ( "people",
            [ StringValue "jack" , StringValue "gill" ]
            |> Array.fromList
            |> ArrayValue
        )
        ]
            |> Dict.fromList
            |> ObjectValue

    evalExp Handlebars.defaultConfig
        ("Hello World"
            |> Text
        )
        value
        --> Ok "Hello World"

    evalExp Handlebars.defaultConfig
        ( (0,["name"])
            |> LookUp
            |> Variable
        )
        value
        --> Ok "jack"

    evalExp Handlebars.defaultConfig
        ( Helper "equals"
            ( LookUp (0,[ "name"])
            , [ LookUp (0,[ "key"]) ]
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

    evalExp Handlebars.defaultConfig
        ( For (0,["people"])
            [(0,[ "@index"]) |> LookUp |> Variable]
        )
        value
        --> Ok "01"

    evalExp Handlebars.defaultConfig
        ( Block "if" (LookUp (0,["valid"]))
            [Text "Hello"]
        )
        value
        --> Ok "Hello"

    evalExp Handlebars.defaultConfig
        ( Block "invalid" (LookUp (0,[]))
            [Text "Hello"]
        )
        value
        --> Err (BlockHelperNotFound "invalid")

-}
evalExp : Config -> Expression -> Value -> Result Error String
evalExp config expression value =
    case expression of
        Text string ->
            Ok string

        Variable subExp ->
            subExp
                |> evalSubExp config value
                |> Result.andThen
                    (\v ->
                        case v of
                            StringValue string ->
                                Ok string

                            _ ->
                                Err (StringExpected ( subExp, v ))
                    )

        For relativePath e ->
            case config.root |> Path.withRelativePath relativePath of
                Just path ->
                    case Value.get path value of
                        Nothing ->
                            PathNotFound path |> Err

                        Just (ArrayValue array) ->
                            array
                                |> Array.toList
                                |> List.indexedMap
                                    (\i _ ->
                                        e
                                            |> List.map
                                                (\it ->
                                                    evalExp
                                                        { config
                                                            | root =
                                                                config.root
                                                                    ++ path
                                                                    ++ [ String.fromInt i ]
                                                        }
                                                        it
                                                        value
                                                )
                                    )
                                |> List.concat
                                |> Result.Extra.combine
                                |> Result.map String.concat

                        Just (ObjectValue dict) ->
                            dict
                                |> Dict.keys
                                |> List.concatMap
                                    (\key ->
                                        e
                                            |> List.map
                                                (\it ->
                                                    evalExp
                                                        { config
                                                            | root =
                                                                config.root
                                                                    ++ path
                                                                    ++ [ key ]
                                                        }
                                                        it
                                                        value
                                                )
                                    )
                                |> Result.Extra.combine
                                |> Result.map String.concat

                        _ ->
                            CollectionExpected config.root relativePath |> Err

                Nothing ->
                    PathNotValid config.root relativePath |> Err

        Block string subExp exp ->
            case config.blockHelpers |> Dict.get string of
                Just fun ->
                    subExp
                        |> evalSubExp config value
                        |> Result.andThen
                            (\arg ->
                                fun
                                    { arg = arg
                                    , throw = \err -> FromBlockHelper { helper = string, error = err }
                                    , content =
                                        \args ->
                                            exp
                                                |> List.map
                                                    (\it ->
                                                        value
                                                            |> evalExp { config | root = args } it
                                                    )
                                                |> Result.Extra.combine
                                                |> Result.map String.concat
                                    }
                                    config.root
                            )

                Nothing ->
                    BlockHelperNotFound string |> Err
