module Bimap exposing
    ( Bimap
    , init
    , variant
    , build
    , fromString
    , toString
    , values
    , toIndex
    , fromIndex
    , compare
    , decoder
    , encoder
    )

{-| Simple bidirectional mapping between Strings and custom types; sorting and comparison; JSON encoding and decoding

@docs Bimap

@docs init

@docs variant

@docs build

@docs fromString

@docs toString

@docs values

@docs toIndex

@docs fromIndex

@docs compare

@docs decoder

@docs encoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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


{-| Returns the `Int` index of a custom type value. Values are indexed in the same order that they were added.

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

    Bimap.toIndex bimap Yes -- 0
    Bimap.toIndex bimap No -- 1

-}
toIndex : Bimap a -> a -> Int
toIndex (Bimap { dict }) value =
    toIndexRec (OrderedDict.values dict) value 0


toIndexRec : List a -> a -> Int -> Int
toIndexRec all value countAccumulator =
    case all of
        [] ->
            countAccumulator

        x :: xs ->
            if x == value then
                countAccumulator

            else
                toIndexRec xs value (countAccumulator + 1)


{-| Returns `Just a` for a custom type value at a given `Int` index, if one exists; `Nothing` otherwise. Values are indexed in the same order that they were added.

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

    Bimap.fromIndex bimap 0 -- Just Yes
    Bimap.fromIndex bimap 1 -- Just No
    Bimap.fromIndex bimap 2 -- Nothing

-}
fromIndex : Bimap a -> Int -> Maybe a
fromIndex (Bimap { dict }) index =
    if index < 0 then
        Nothing

    else
        fromIndexRec (OrderedDict.values dict) index 0


fromIndexRec : List a -> Int -> Int -> Maybe a
fromIndexRec all index countAccumulator =
    case all of
        [] ->
            Nothing

        x :: xs ->
            if index == countAccumulator then
                Just x

            else
                fromIndexRec xs index (countAccumulator + 1)


{-| Compares two custom type values in a `Bimap`. Very useful for sorting when partially applied and passed to [`List.sortWith`](https://package.elm-lang.org/packages/elm/core/latest/List#sortWith).

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

    sort : List Response -> List Response
    sort =
        List.sortWith (Bimap.compare bimap)


    sort [ No, Yes ] -- [ Yes, No ]

-}
compare : Bimap a -> a -> a -> Order
compare bimap a b =
    Basics.compare (toIndex bimap a) (toIndex bimap b)


{-| Decoder for a custom type value in a `Bimap`.

    import Json.Decode as Decode exposing (Decoder)

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

    decoder : Decoder Response
    decoder =
        Bimap.decoder bimap

    okJson : String
    okJson =
        """{ "response": "Yes" }"""

    badJson : String
    badJson =
        """{ "response": "Maybe" }"""

    jsonDecoder : Decoder Response
    jsonDecoder =
        Decode.field "response" decoder

    Decode.decodeString jsonDecoder okJson -- Ok Yes
    Decode.decodeString jsonDecoder badJson -- Err (Field "number" (Failure "Decode failed; Six is not a valid value. Expected one of: One; Two; Three" <internals>))


    sort [ No, Yes ] -- [ Yes, No ]

-}
decoder : Bimap a -> Decoder a
decoder bimap =
    Decode.andThen
        (\value ->
            fromString bimap value
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault
                    (Decode.fail
                        (String.join " "
                            [ "Decode failed;"
                            , value
                            , "is not a valid value. Expected one of:"
                            , String.join "; " (values bimap |> List.map Tuple.first)
                            ]
                        )
                    )
        )
        Decode.string


{-| Encoder for a custom type value in a `Bimap`.

    import Json.Encode as Encode

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


    encoder : Response -> Encode.Value
    encoder =
        Bimap.encoder bimap

    Encoder.encode 0 (encoder Yes) -- "\"Yes\""

-}
encoder : Bimap a -> a -> Encode.Value
encoder bimap value =
    toString bimap value |> Encode.string
