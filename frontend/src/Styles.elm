module Styles exposing (..)

import Colors
import Element as E exposing (Attribute)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


explain =
    E.explain Debug.todo



---- Utils ----


buttonStyles : List (Attribute msg)
buttonStyles =
    [ Background.color Colors.primary
    , Font.color Colors.white
    , E.paddingXY 20 10
    , Border.rounded 5
    , E.focused [ Background.color Colors.primaryDark ]
    , E.mouseOver [ Background.color Colors.primaryDark ]
    ]


borderBottomLight : Attribute msg
borderBottomLight =
    Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }


navbarStyles : List (Attribute msg)
navbarStyles =
    [ E.spaceEvenly
    , E.width E.fill
    , E.paddingXY 10 10
    , navbarShadow
    , E.height <| E.px 70
    , Border.color <| Colors.black
    , Border.width 2
    ]


searchbarStyles : List (Attribute msg)
searchbarStyles =
    [ E.centerX ]


navbarShadow : Attribute msg
navbarShadow =
    Border.shadow
        { offset = ( 1.0, 1.0 )
        , size = 2.0
        , blur = 2.0
        , color = Colors.gray
        }



---- Question Details Page ----


questionDetailsPageStyle : List (Attribute msg)
questionDetailsPageStyle =
    [ E.width E.fill
    , E.paddingXY 0 50
    , E.height E.fill
    , Background.color <| Colors.bg
    ]


contentBoxStyles : List (E.Attribute msg)
contentBoxStyles =
    [ E.centerX
    , E.alignTop
    , Border.color <| Colors.primaryDark
    , Border.width 1
    , E.width (E.maximum 800 <| E.px 1200)
    , questionBoxShadow
    , E.paddingEach { top = 20, right = 20, left = 20, bottom = 20 }
    ]


questionBox : List (E.Attribute msg)
questionBox =
    [ E.centerX
    , E.width E.fill
    , E.spacingXY 20 20
    , E.paddingXY 0 20
    , Border.color <| Colors.black
    , borderBottomLight
    ]


questionBoxShadow : Attribute msg
questionBoxShadow =
    Border.shadow
        { offset = ( 1, 1 )
        , size = 1.0
        , blur = 8.0
        , color = Colors.black
        }


titleStyles : List (Attribute msg)
titleStyles =
    [ Font.bold
    , Font.size 25
    , Font.alignLeft
    ]


contentStyles : List (Attribute msg)
contentStyles =
    [ Font.size 20
    , Font.color Colors.gray
    , Font.alignLeft
    ]


subTextStyles : List (Attribute msg)
subTextStyles =
    [ Font.size 16
    , Font.italic
    , Font.light
    , Font.color Colors.gray
    , Font.alignLeft
    ]


answerDisplay : List (Attribute msg)
answerDisplay =
    [ borderBottomLight
    , E.width E.fill
    , E.paddingXY 0 20
    , E.spacingXY 0 10
    ]


answerBox : List (Attribute msg)
answerBox =
    [ E.centerX
    , E.width E.fill
    , E.paddingXY 10 30
    , E.spacingXY 0 40
    ]
