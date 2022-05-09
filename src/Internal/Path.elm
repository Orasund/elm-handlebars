module Internal.Path exposing (..)

import Json.Decode exposing (string)
import List exposing (tail)


type alias Path =
    List String


type alias RelativePath =
    ( Int, List String )


{-|

    (0,[])
        |> relativeToString
        --> "."

    (1,[])
        |> relativeToString
        --> "../."

    (0,["a"])
        |> relativeToString
        --> "a"

    (-1,["a"])
        |> relativeToString
        --> "a"

    (1,["a"])
        |> relativeToString
        --> "../a"

    (2,[])
        |> relativeToString
        --> "../../."

    (0,["a", "b"])
        |> relativeToString
        --> "a.b"

    (1,["a", "b"])
        |> relativeToString
        --> "../a.b"

-}
relativeToString : RelativePath -> String
relativeToString ( int, list ) =
    (List.repeat int "../" |> String.concat)
        ++ (if List.isEmpty list then
                "."

            else
                list |> String.join "."
           )


toString : Path -> String
toString list =
    relativeToString ( 0, list )


{-| Turns relative path to path

    [ "people", "jack"]
        |> withRelativePath (-1,[])
        --> Nothing

    [ "people", "jack"]
        |> withRelativePath (0,[])
        --> Just ["people","jack"]

    [ "people","jack"]
        |> withRelativePath (1,[ "gill"])
        --> Just ["people","gill"]

    [ "people","jack" ]
        |> withRelativePath (2,[ "cities" ])
        --> Just [ "cities"]


    [ "people","jack"]
        |> withRelativePath (3,[])
        --> Nothing

-}
withRelativePath : RelativePath -> Path -> Maybe Path
withRelativePath ( back, forward ) list =
    let
        length =
            list |> List.length
    in
    if back < 0 || length < back then
        Nothing

    else
        (list |> List.take (length - back))
            ++ forward
            |> Just
