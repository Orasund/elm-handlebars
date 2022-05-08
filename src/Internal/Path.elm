module Internal.Path exposing (..)


type alias Path =
    List String


type alias RelativePath =
    List (Maybe String)


toString : Path -> String
toString list =
    String.join "." list


{-| Turns relative path to path

    [ "people", "jack"]
        |> withRelativePath []
        --> Just ["people","jack"]

    [ "people","jack"]
        |> withRelativePath [Nothing, Just "gill"]
        --> Just ["people","gill"]

    [ "people","jack" ]
        |> withRelativePath [ Nothing, Nothing, Just "cities" ]
        --> Just [ "cities"]


    [ "people","jack"]
        |> withRelativePath [Nothing,Nothing,Nothing]
        --> Nothing

-}
withRelativePath : RelativePath -> Path -> Maybe Path
withRelativePath relative list =
    let
        rec r l =
            case ( r, l ) of
                ( Nothing :: _, [] ) ->
                    Nothing

                ( Nothing :: rTail, head :: tail ) ->
                    rec rTail tail

                ( (Just a) :: rTail, _ ) ->
                    rec rTail (a :: l)

                ( [], _ ) ->
                    Just (List.reverse l)
    in
    rec relative (List.reverse list)
