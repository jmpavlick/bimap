module Tests exposing (..)

import Bimap exposing (Bimap)
import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (Test)
import Test.Runner exposing (Runner)


type Number
    = One
    | Two
    | Three


bimap : Bimap Number
bimap =
    Bimap.init
        (\one two three value ->
            case value of
                One ->
                    one

                Two ->
                    two

                Three ->
                    three
        )
        |> Bimap.variant "One" One
        |> Bimap.variant "Two" Two
        |> Bimap.variant "Three" Three
        |> Bimap.build


allValues : List Number
allValues =
    let
        next : List Number -> List Number
        next list =
            case List.head list of
                Nothing ->
                    Three :: list |> next

                Just Three ->
                    Two :: list |> next

                Just Two ->
                    One :: list |> next

                Just One ->
                    list
    in
    next []


allStrings : List String
allStrings =
    [ "One", "Two", "Three" ]


toStringTest : Test
toStringTest =
    Test.test "All values of Number should map to the correct string" <|
        \() ->
            List.map (Bimap.toString bimap) allValues
                |> Expect.equalLists allStrings


fromStringTest : Test
fromStringTest =
    Test.test "All String values should map to the correct Number" <|
        \() ->
            List.map (Bimap.fromString bimap) allStrings
                |> Expect.equalLists (List.map Just allValues)


fromStringFailureTest : Test
fromStringFailureTest =
    Test.test "Bimap.fromString should return Nothing for a String that's not in the Bimap" <|
        \() ->
            Bimap.fromString bimap "Four" |> Expect.equal Nothing


valuesTest : Test
valuesTest =
    Test.test "Bimap.values should return a List (String, a) of all of its keys and values" <|
        \() ->
            Bimap.values bimap |> List.unzip |> Expect.equal ( allStrings, allValues )


toIndexTest : Test
toIndexTest =
    Test.test "Bimap.toIndex should return an Int for the index of a value in its collection" <|
        \() ->
            Expect.equalLists [ 0, 1, 2 ]
                [ Bimap.toIndex bimap One
                , Bimap.toIndex bimap Two
                , Bimap.toIndex bimap Three
                ]


fromIndexTest : Test
fromIndexTest =
    Test.test "Bimap.fromIndex should return a Just a for a custom type element at a valid index" <|
        \() ->
            Expect.equalLists [ Just One, Just Two, Just Three ]
                [ Bimap.fromIndex bimap 0
                , Bimap.fromIndex bimap 1
                , Bimap.fromIndex bimap 2
                ]


fromIndexFailureTest : Test
fromIndexFailureTest =
    Test.test "Bimap.fromIndex should return a Nothing for an out-of-range index value" <|
        \() ->
            Bimap.fromIndex bimap 4 |> Expect.equal Nothing


compareTest : Test
compareTest =
    Test.test "Bimap.compare should compare two values in its collection and return an Order, based on the custom type's order in the Bimap match function" <|
        \() ->
            Expect.equalLists [ LT, EQ, GT ]
                [ Bimap.compare bimap One Two
                , Bimap.compare bimap Three Three
                , Bimap.compare bimap Two One
                ]


decoderTest : Test
decoderTest =
    let
        decoder : Decoder Number
        decoder =
            Bimap.decoder bimap

        jsonDecoder : Decoder Number
        jsonDecoder =
            Decode.field "number" decoder

        run : String -> Result Decode.Error Number
        run =
            Decode.decodeString jsonDecoder
    in
    Test.describe "Bimap.decoder should generate a decoder based on a Bimap"
        [ Test.test "A valid JSON value should be decoded correctly" <|
            \() ->
                run """{ "number": "One" }""" |> Expect.equal (Ok One)
        , Test.test "An invalid JSON value should return the correct failure message" <|
            \() ->
                let
                    errorMessage : String
                    errorMessage =
                        "Problem with the value at json.number:\n\n    \"Six\"\n\nDecode failed; Six is not a valid value. Expected one of: One; Two; Three"
                in
                case run """{ "number": "Six" }""" of
                    Ok _ ->
                        Expect.fail "decoding this value should fail!"

                    Err e ->
                        Decode.errorToString e |> Expect.equal errorMessage
        ]


encoderTest : Test
encoderTest =
    Test.test "Bimap.encoder should generate a valid JSON encoder based on a Bimap" <|
        \() ->
            let
                encoder : Number -> Encode.Value
                encoder =
                    Bimap.encoder bimap
            in
            Encode.encode 0 (encoder Two) |> Expect.equal "\"Two\""
