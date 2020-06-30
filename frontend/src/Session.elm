module Session exposing (..)

import Browser.Navigation as Nav


type alias Session =
    { key : Nav.Key
    , currentUser : Maybe CurrentUser
    }


type alias CurrentUser =
    { firstName : String
    , lastName : String
    }
