module Styles exposing (..)

import Colors
import Element as E exposing (Attribute)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


buttonStyles : List (Attribute msg)
buttonStyles =
    [ Background.color Colors.primary
    , Font.color Colors.white
    , E.paddingXY 20 10
    , Border.rounded 5
    , E.focused [ Background.color Colors.primaryDark ]
    , E.mouseOver [ Background.color Colors.primaryDark ]
    ]
