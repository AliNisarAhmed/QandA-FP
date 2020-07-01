module Utils exposing (..)

import DateFormat as DF
import Element as E exposing (Attribute, Element)
import Http
import Iso8601 as Iso
import Session exposing (Session, UserId(..), getId)
import Time


displayTime : String -> String
displayTime time =
    parseTime time
        |> DF.format
            [ DF.monthNameFull
            , DF.text " "
            , DF.dayOfMonthNumber
            , DF.text ", "
            , DF.yearNumber
            ]
            Time.utc


parseTime : String -> Time.Posix
parseTime =
    Result.withDefault (Time.millisToPosix 2000) << Iso.toTime


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.BadBody str ->
            str

        Http.Timeout ->
            "Time out"

        Http.BadUrl str ->
            str

        Http.BadStatus int ->
            "bad status: " ++ String.fromInt int

        Http.NetworkError ->
            "Network Error"


displayErrorText : Maybe String -> Element msg
displayErrorText me =
    case me of
        Nothing ->
            E.none

        Just error ->
            E.el [] <| E.text error


hideElementForGuest : Session -> Element msg -> Element msg
hideElementForGuest s e =
    case s.currentUser of
        Nothing ->
            E.none

        Just _ ->
            e


hideElementForUser : Session -> UserId -> Element msg -> Element msg
hideElementForUser s (UserId id) element =
    case s.currentUser of
        Nothing ->
            E.none

        Just cu ->
            if getId cu.id /= id then
                E.none

            else
                element
