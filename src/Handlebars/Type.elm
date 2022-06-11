module Handlebars.Type exposing (Type(..), normalize, ofTemplate)

import Dict exposing (Dict)
import Handlebars.Expression exposing (Expression(..), SubExp(..))
import Handlebars.Path as Path exposing (Path)


type Type
    = ArrayType (Maybe Type)
    | DictType (Dict String (Maybe Type))
    | Any


set : Path -> Maybe Type -> Maybe Type -> Maybe Type
set path maybeValue maybeType =
    case path of
        [] ->
            case ( maybeValue, maybeType ) of
                ( _, Nothing ) ->
                    maybeValue

                ( Nothing, _ ) ->
                    maybeType

                ( Just v, Just t ) ->
                    if v == t then
                        Just v

                    else
                        Just Any

        "@index" :: tail ->
            case maybeType of
                Nothing ->
                    Nothing
                        |> set tail maybeValue
                        |> ArrayType
                        |> Just

                Just (ArrayType t) ->
                    t
                        |> set tail maybeValue
                        |> ArrayType
                        |> Just

                _ ->
                    Any
                        |> Just

        head :: tail ->
            case maybeType of
                Nothing ->
                    Nothing
                        |> set tail maybeValue
                        |> Dict.singleton head
                        |> DictType
                        |> Just

                Just (DictType d) ->
                    d
                        |> Dict.update head
                            (\maybe ->
                                maybe
                                    |> Maybe.andThen identity
                                    |> set tail maybeValue
                                    |> Just
                            )
                        |> DictType
                        |> Just

                Just _ ->
                    Any |> Just


normalize : Dict Path (Maybe ()) -> Maybe Type
normalize =
    Dict.foldl
        (\path maybeUnit ->
            set path (maybeUnit |> Maybe.map (\() -> Any))
        )
        Nothing


ofTemplate : List Expression -> Result ( Path, Path.RelativePath ) (Dict Path (Maybe ()))
ofTemplate =
    List.foldl
        (\exp ->
            Result.map2 unifyContexts
                (exp |> ofExp [])
        )
        (Ok Dict.empty)


ofExp : Path -> Expression -> Result ( Path, Path.RelativePath ) (Dict Path (Maybe ()))
ofExp path exp =
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
                            |> List.map (p ++ [ "@index" ] |> ofExp)
                            |> List.foldl (Result.map2 unifyContexts) (Ok Dict.empty)
                    )
                |> Maybe.withDefault (( path, relativePath ) |> Err)

        Block _ _ _ ->
            Ok Dict.empty


typeOfSupExp : Path -> SubExp -> Result ( Path, Path.RelativePath ) (Dict Path (Maybe ()))
typeOfSupExp path subExp =
    case subExp of
        LookUp relativePath ->
            path
                |> Path.withRelativePath relativePath
                |> Maybe.map (\p -> Dict.singleton p Nothing |> Ok)
                |> Maybe.withDefault (( path, relativePath ) |> Err)

        Helper _ ( head, tail ) ->
            head
                :: tail
                |> List.map (typeOfSupExp path)
                |> List.foldl (Result.map2 unifyContexts) (Ok Dict.empty)


unifyContexts : Dict Path (Maybe ()) -> Dict Path (Maybe ()) -> Dict Path (Maybe ())
unifyContexts d1 d2 =
    Dict.merge Dict.insert
        (\k v1 v2 ->
            Dict.insert k
                (case ( v1, v2 ) of
                    ( Nothing, _ ) ->
                        v2

                    ( _, Nothing ) ->
                        v1

                    ( Just (), Just () ) ->
                        Just ()
                )
        )
        Dict.insert
        d1
        d2
        Dict.empty
