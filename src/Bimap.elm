module Bimap exposing (Bimap, build, fromString, init, toString, variant)

import Dict exposing (Dict)


type Bimap a
    = Bimap
        { toString : a -> String
        , fromString : String -> Maybe a
        }


type Builder match v
    = Builder
        { match : match
        , dict : Dict String v
        }


init : match -> Builder match v
init match =
    Builder
        { match = match
        , dict = Dict.empty
        }


variant : String -> v -> Builder (String -> b) v -> Builder b v
variant label value (Builder { match, dict }) =
    Builder
        { match = match label
        , dict = Dict.insert label value dict
        }


build : Builder (a -> String) a -> Bimap a
build (Builder { match, dict }) =
    Bimap
        { toString = match
        , fromString = \str -> Dict.get str dict
        }


toString : Bimap a -> a -> String
toString (Bimap b) =
    b.toString


fromString : Bimap a -> String -> Maybe a
fromString (Bimap b) =
    b.fromString
