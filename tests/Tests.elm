module Tests exposing (suite)

import Dict
import Engage.Localization as Localization exposing (Localization)
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
            , test "Array with single object with key and value" <|
                \_ ->
                    """[{"key":"Name.Text","value":"Title"}]"""
                        |> Decode.decodeString Localization.decoder
                        |> Result.withDefault Dict.empty
                        |> (\localization -> { localization = localization })
                        |> Localization.localizeString "Name"
                        |> Expect.equal "Title"
            , test "Array with single object with Key and Value" <|
                \_ ->
                    """[{"Key":"Name.Text","Value":"Title"}]"""
                        |> Decode.decodeString Localization.decoder
                        |> Result.withDefault Dict.empty
                        |> (\localization -> { localization = localization })
                        |> Localization.localizeString "Name"
                        |> Expect.equal "Title"
            , test "Object with properties" <|
                \_ ->
                    """{"Name.Text":"Title"}"""
                        |> Decode.decodeString Localization.decoder
                        |> Result.withDefault Dict.empty
                        |> (\localization -> { localization = localization })
                        |> Localization.localizeString "Name"
                        |> Expect.equal "Title"
            , fuzz entriesFuzzer "Decode entries from [{key,value},…] format" <|
                \entries ->
                    localizationTest
                        (encodeEntriesAsLowerCaseList entries)
                        (localizeEntries entries)
                        (Expect.equalLists entries)
            , fuzz entriesFuzzer "Decode entries from [{Key,Value},…] format" <|
                \entries ->
                    localizationTest
                        (encodeEntriesAsLowerCaseList entries)
                        (localizeEntries entries)
                        (Expect.equalLists entries)
            , fuzz entriesFuzzer "Decode entries from {key:value,…} format" <|
                \entries ->
                    localizationTest
                        (encodeEntriesAsLowerCaseList entries)
                        (localizeEntries entries)
                        (Expect.equalLists entries)
            ]
        , describe "fromDict"
            [ test "Empty dict is empty" <|
                \_ ->
                    Dict.empty
                        |> Localization.fromDict
                        |> Expect.equalDicts Dict.empty
            , test "Empty dict does not match value" <|
                \_ ->
                    Dict.empty
                        |> Localization.fromDict
                        |> Model
                        |> Localization.localizeStringWithDefault "Not Found" "Name.Text"
                        |> Expect.equal "Not Found"
            , test "Can retrieve value from dict without matching case" <|
                \_ ->
                    Dict.fromList [ ( "Name.Text", "The Name" ) ]
                        |> Localization.fromDict
                        |> Model
                        |> Localization.localizeStringWithDefault "Not Found" "name.text"
                        |> Expect.equal "The Name"
            , test "Can retrieve value from dict without matching .Text suffix" <|
                \_ ->
                    Dict.fromList [ ( "Name.Text", "The Name" ) ]
                        |> Localization.fromDict
                        |> Model
                        |> Localization.localizeStringWithDefault "Not Found" "Name"
                        |> Expect.equal "The Name"
            ]
        , describe "empty"
            [ test "Empty Localization is empty dict" <|
                \_ ->
                    Localization.empty
                        |> Expect.equalDicts Dict.empty
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


localizationTest : Encode.Value -> (Localization -> a) -> (a -> Expect.Expectation) -> Expect.Expectation
localizationTest encodedEntries getValue expectation =
    encodedEntries
        |> Decode.decodeValue Localization.decoder
        |> Result.map getValue
        |> Result.map expectation
        |> Result.mapError Decode.errorToString
        |> Result.mapError Expect.fail
        |> Result.Extra.merge


type alias Model =
    { localization : Localization
    }


localizationModelTest : String -> List ( String, String ) -> (Model -> a) -> (a -> Expect.Expectation) -> Test
localizationModelTest description entries getValue expectation =
    let
        wrapLocalization : Localization -> a
        wrapLocalization localization =
            getValue (Model localization)
    in
    describe description
        [ test (description ++ " [{key,value},…] encoding") <|
            \_ ->
                localizationTest
                    (encodeEntriesAsLowerCaseList entries)
                    wrapLocalization
                    expectation
        , test (description ++ " [{Key,Value},…] encoding") <|
            \_ ->
                localizationTest
                    (encodeEntriesAsUpperCaseList entries)
                    wrapLocalization
                    expectation
        , test (description ++ " {key:value,…} encoding") <|
            \_ ->
                localizationTest
                    (encodeEntriesAsObject entries)
                    wrapLocalization
                    expectation
        ]


localizeEntries : List ( String, String ) -> Localization -> List ( String, String )
localizeEntries entries localization =
    let
        model : Model
        model =
            Model localization
    in
    entries
        |> List.map Tuple.first
        |> List.map (\key -> ( key, Localization.localizeString key model ))


encodeEntriesAsLowerCaseList : List ( String, String ) -> Encode.Value
encodeEntriesAsLowerCaseList entries =
    let
        encodeEntry : ( String, String ) -> Encode.Value
        encodeEntry ( key, value ) =
            Encode.object [ ( "key", Encode.string key ), ( "value", Encode.string value ) ]
    in
    entries
        |> Encode.list encodeEntry


encodeEntriesAsUpperCaseList : List ( String, String ) -> Encode.Value
encodeEntriesAsUpperCaseList entries =
    let
        encodeEntry : ( String, String ) -> Encode.Value
        encodeEntry ( key, value ) =
            Encode.object [ ( "Key", Encode.string key ), ( "Value", Encode.string value ) ]
    in
    entries
        |> Encode.list encodeEntry


encodeEntriesAsObject : List ( String, String ) -> Encode.Value
encodeEntriesAsObject entries =
    let
        encodeEntry : ( String, String ) -> ( String, Encode.Value )
        encodeEntry ( key, value ) =
            ( key, Encode.string value )
    in
    entries
        |> List.map encodeEntry
        |> Encode.object


entriesFuzzer : Fuzzer (List ( String, String ))
entriesFuzzer =
    let
        entryFuzzer : Fuzzer ( String, String )
        entryFuzzer =
            Fuzz.tuple ( Fuzz.string, Fuzz.string )

        removeDuplicates : List ( String, String ) -> List ( String, String )
        removeDuplicates entries =
            entries
                |> List.Extra.gatherEqualsBy (Tuple.first >> String.toUpper)
                |> List.map Tuple.first
    in
    entryFuzzer
        |> Fuzz.list
        |> Fuzz.map removeDuplicates
