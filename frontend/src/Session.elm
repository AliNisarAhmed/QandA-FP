module Session exposing (..)

import Browser.Navigation as Nav


type alias Session =
    { key : Nav.Key
    , currentUser : Maybe CurrentUser
    }


type alias CurrentUser =
    { id : UserId
    , firstName : String
    , lastName : String
    }


type UserId
    = UserId Int


getId : UserId -> Int
getId (UserId id) =
    id
