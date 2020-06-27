module Utils exposing (..)

import DateFormat as DF
import Iso8601 as Iso
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
