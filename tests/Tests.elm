module Tests exposing (suite)

import Bimap exposing (Bimap)
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Bimap tests"
        [ toStringTest
        , fromStringTest
        , fromStringFailureTest
        , valuesTest
        ]


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
