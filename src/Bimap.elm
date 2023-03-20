module Bimap exposing
    ( Bimap
    , init
    , variant
    , build
    , fromString
    , toString
    , values
    )

{-| Simple bidirectional mapping between Strings and custom types.

@docs Bimap

@docs init

@docs variant

@docs build

@docs fromString

@docs toString

@docs values

-}

import OrderedDict exposing (OrderedDict)


{-| A bidirectional map between a `String` and a custom type.
-}
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


{-| Initializes a `Bimap`. You will need to pass a pattern-matching function
with one parameter per variant, plus an extra parameter for the "value".

    type Response
        = Yes
        | No

    bimap : Bimap Response
    bimap =
        Bimap.init
            (\yes no value ->
                case value of
                    Yes ->
                        yes

                    No ->
                        no
            )
            |> Bimap.variant "Yes" Yes
            |> Bimap.variant "No" No
            |> Bimap.build

-}
init : match -> Builder match v
init match =
    Builder
        { match = match
        , dict = OrderedDict.empty
        }


{-| Adds a map between a `String` and a value of your custom type.

    ... -- a pattern-matching function goes before the variants
    |> Bimap.variant "Yes" Yes
    |> Bimap.variant "No" No
    ... -- a call to Bimap.build goes after

-}
variant : String -> v -> Builder (String -> b) v -> Builder b v
variant label value (Builder { match, dict }) =
    Builder
        { match = match label
        , dict = OrderedDict.insert label value dict
        }


{-| Resolves your maps and pattern-matching function into
a `Bimap` that you can use.

    ... -- calls to Bimap.variant go before the call to Bimap.build
    |> Bimap.build

-}
build : Builder (a -> String) a -> Bimap a
build (Builder { match, dict }) =
    Bimap
        { toString = match
        , fromString = \str -> OrderedDict.get str dict
        , dict = dict
        }


{-| Returns the mapped `String` for a given custom type value.

    type Response
        = Yes
        | No

    bimap :
        Bimap Response
    bimap =
        Bimap.init
            (\yes no value ->
                case value of
                    Yes ->
                        yes

                    No ->
                        no
            )
            |> Bimap.variant "Yes" Yes
            |> Bimap.variant "No" No
            |> Bimap.build


    Bimap.toString No --> "No"

-}
toString : Bimap a -> a -> String
toString (Bimap b) =
    b.toString


{-| Returns the mapped custom type value for a given `String`,
if the `String` has been mapped.

    type Response
        = Yes
        | No

    bimap : Bimap Response
    bimap =
        Bimap.init
            (\yes no value ->
                case value of
                    Yes ->
                        yes

                    No ->
                        no
            )
            |> Bimap.variant "Yes" Yes
            |> Bimap.variant "No" No
            |> Bimap.build


    Bimap.fromString bimap "Yes" --> Yes
    Bimap.fromString bimap "Maybe" --> Nothing

-}
fromString : Bimap a -> String -> Maybe a
fromString (Bimap b) =
    b.fromString


{-| Returns a `List` of tuples of all `String` keys and custom type values, in the same order that they were added.

    type Response
        = Yes
        | No

    bimap : Bimap Response
    bimap =
        Bimap.init
            (\yes no value ->
                case value of
                    Yes ->
                        yes

                    No ->
                        no
            )
            |> Bimap.variant "Yes" Yes
            |> Bimap.variant "No" No
            |> Bimap.build


    Bimap.values bimap -- [ ( "Yes", Yes ), ( "No", No ) ]

-}
values : Bimap a -> List ( String, a )
values (Bimap { dict }) =
    OrderedDict.toList dict
