module Handlebars.Checker exposing (..)

import Dict exposing (Dict)
import Handlebars.Expression exposing (Expression(..), SubExp(..))
import Handlebars.Path as Path exposing (Path)
import List.Extra


type Type
    = StringType
    | BooleanType
    | ArrayType Type
    | DictType (Dict String Type)
    | UnionType (List Type)
    | Any


type Error
    = InvalidRelativePath Path Path.RelativePath


typeOfExp : Path -> Expression -> Result Error (Dict Path (Maybe Type))
typeOfExp path exp =
    case exp of
        Text _ ->
            Ok Dict.empty

        Variable supExp ->
            supExp
                |> typeOfSupExp path

        For relativePath list ->
            path
                |> Path.withRelativePath relativePath
                |> Maybe.map
                    (\p ->
                        list
                            |> List.map (typeOfExp p)
                            |> List.foldl (Result.map2 unifyContexts) (Ok Dict.empty)
                    )
                |> Maybe.withDefault (InvalidRelativePath path relativePath |> Err)

        Block _ _ _ ->
            Ok Dict.empty


typeOfSupExp : Path -> SubExp -> Result Error (Dict Path (Maybe Type))
typeOfSupExp path subExp =
    case subExp of
        LookUp relativePath ->
            path
                |> Path.withRelativePath relativePath
                |> Maybe.map (\p -> Dict.singleton p Nothing |> Ok)
                |> Maybe.withDefault (InvalidRelativePath path relativePath |> Err)

        Helper _ ( head, tail ) ->
            head
                :: tail
                |> List.map (typeOfSupExp path)
                |> List.foldl (Result.map2 unifyContexts) (Ok Dict.empty)


unifyTypes : Maybe Type -> Maybe Type -> Maybe Type
unifyTypes maybe1 maybe2 =
    case ( maybe1, maybe2 ) of
        ( Nothing, _ ) ->
            maybe2

        ( _, Nothing ) ->
            maybe1

        ( Just Any, _ ) ->
            Just Any

        ( _, Just Any ) ->
            Just Any

        ( Just (UnionType l1), Just (UnionType l2) ) ->
            l1
                ++ l2
                |> List.Extra.unique
                |> UnionType
                |> Just

        ( Just (UnionType l1), Just t2 ) ->
            t2
                :: l1
                |> List.Extra.unique
                |> UnionType
                |> Just

        ( Just t1, Just (UnionType l2) ) ->
            t1
                :: l2
                |> List.Extra.unique
                |> UnionType
                |> Just

        ( Just t1, Just t2 ) ->
            [ t1, t2 ]
                |> List.Extra.unique
                |> UnionType
                |> Just


unifyContexts : Dict Path (Maybe Type) -> Dict Path (Maybe Type) -> Dict Path (Maybe Type)
unifyContexts d1 d2 =
    Dict.merge Dict.insert
        (\k v1 v2 -> Dict.insert k (unifyTypes v1 v2))
        Dict.insert
        d1
        d2
        Dict.empty
