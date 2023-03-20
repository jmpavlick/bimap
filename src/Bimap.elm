module Bimap exposing (Bimap, build, fromString, init, toString, values, variant)

import OrderedDict exposing (OrderedDict)


type Bimap a
    = Bimap
        { toString : a -> String
        , fromString : String -> Maybe a
        , dict : OrderedDict String a
        }


type Builder match v
    = Builder
        { match : match
        , dict : OrderedDict String v
        }


init : match -> Builder match v
init match =
    Builder
        { match = match
        , dict = OrderedDict.empty
        }


variant : String -> v -> Builder (String -> b) v -> Builder b v
variant label value (Builder { match, dict }) =
    Builder
        { match = match label
        , dict = OrderedDict.insert label value dict
        }


build : Builder (a -> String) a -> Bimap a
build (Builder { match, dict }) =
    Bimap
        { toString = match
        , fromString = \str -> OrderedDict.get str dict
        , dict = dict
        }


toString : Bimap a -> a -> String
toString (Bimap b) =
    b.toString


fromString : Bimap a -> String -> Maybe a
fromString (Bimap b) =
    b.fromString


values : Bimap a -> List ( String, a )
values (Bimap { dict }) =
    OrderedDict.toList dict
