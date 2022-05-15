module Introduction.WhatIsHandlebar exposing (..)

import ElmBook.Chapter as Chapter exposing (Chapter)


page : Chapter msg
page =
    Chapter.chapter "What is Handlebars?"
        |> Chapter.render """
Handlebars is as simple templating language.

It uses a template and a json input to generate a String.

    <p>{{fristname}} {{lastname}}</p>

A handlebars expression is a `{{`, some contents followed by a `}}`.
These expression will be then replaces with values from the json.

# Installation & First Steps

To install, type in `elm install Orasund/elm-handlebars`.
If you will need to work with json: `elm install elm/json`.

You can now start using the language by providing a json file and a template.alias

    case 
        "{"firstname":"Yehuda","lastname":"Katz"}"
        |> Json.Decode.decodeString Json.Decode.value
    of
        Ok value ->
            case
                value
                |> Handlebars.compile Handlebars.defaultConfig
                    "<p>{{fristname}} {{lastname}}</p>"
            of
            Just string ->
                string
            Err err ->
                err
                    |> Handlebars.errorToString
                
        Err err ->
            Json.Decode.errorToString err


"""
