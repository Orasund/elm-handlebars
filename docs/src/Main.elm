module Main exposing (main)

import ElmBook exposing (Book)
import ElmBook.ThemeOptions
import Introduction.WhatIsHandlebar


main : Book ()
main =
    ElmBook.book "Elm-Handlebars"
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation
            ]
        |> ElmBook.withChapters
            [ Introduction.WhatIsHandlebar.page
            ]
