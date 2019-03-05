module Tests exposing (suite)

import Dict exposing (Dict)
import Engage.Localization as Localization
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Result.Extra
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Elm DNN Localization"
        [ describe "JSON decoding"
            [ test "Empty array" <|
                \_ ->
                    "[]"
                        |> Decode.decodeString Localization.decoder
                        |> Expect.equal (Ok Dict.empty)
            , fuzz entriesFuzzer "Decode entries" <|
                \entries ->
                    localizationTest entries
                        (localizeEntries entries)
                        (Expect.equalDicts (Dict.fromList entries))
            ]
        , describe "localizeString"
            [ localizationModelTest "Missing key defaults to brackets"
                []
                (Localization.localizeString "foobar")
                (Expect.equal "[foobar]")
            , localizationModelTest "Key matches without .Text suffix"
                [ ( "foo", "bar" ) ]
                (Localization.localizeString "foo")
                (Expect.equal "bar")
            , localizationModelTest "Key match is case insensitive"
                [ ( "foo", "bar" ) ]
                (Localization.localizeString "Foo")
                (Expect.equal "bar")
            ]
        ]


localizationTest entries getValue expectation =
    entries
        |> encodeEntries
        |> Decode.decodeValue Localization.decoder
        |> Result.map getValue
        |> Result.map expectation
        |> Result.mapError Decode.errorToString
        |> Result.mapError Expect.fail
        |> Result.Extra.merge


localizationModelTest description entries getValue expectation =
    test description <|
        \_ ->
            localizationTest
                entries
                (\localization -> getValue { localization = localization })
                expectation


localizeEntries : List ( String, String ) -> Localization.Localization -> Dict String String
localizeEntries entries localization =
    let
        model =
            { localization = localization }
    in
    entries
        |> List.map Tuple.first
        |> List.map (\key -> ( key, Localization.localizeString key model ))
        |> Dict.fromList


encodeEntries : List ( String, String ) -> Encode.Value
encodeEntries entries =
    let
        encodeEntry ( key, value ) =
            Encode.object [ ( "key", Encode.string key ), ( "value", Encode.string value ) ]
    in
    entries
        |> Encode.list encodeEntry


entriesFuzzer : Fuzzer (List ( String, String ))
entriesFuzzer =
    let
        entryFuzzer =
            Fuzz.tuple ( Fuzz.string, Fuzz.string )

        removeDuplicates entries =
            entries
                |> List.Extra.gatherEqualsBy (Tuple.first >> String.toUpper)
                |> List.map Tuple.first
    in
    entryFuzzer
        |> Fuzz.list
        |> Fuzz.map removeDuplicates
