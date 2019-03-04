module Tests exposing (suite)

import Dict
import Engage.Localization as Localization
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
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
                    Debug.log "entries" entries
                        |> encodeEntries
                        |> Decode.decodeValue Localization.decoder
                        |> Result.map Dict.toList
                        |> Result.map Set.fromList
                        |> Result.map (Set.diff (Set.fromList entries))
                        |> Result.map (Expect.equalSets Set.empty)
                        |> Result.mapError Decode.errorToString
                        |> Result.mapError Expect.fail
                        |> merge
                        |> Debug.log "expectation"
            ]
        ]


encodeEntries : List ( String, String ) -> Encode.Value
encodeEntries entries =
    entries
        |> Encode.list encodeEntry

encodeEntry ( key, value ) =
    Encode.object [ ( "key", Encode.string key ), ( "value", Encode.string value ) ]


entriesFuzzer : Fuzzer (List ( String, String ))
entriesFuzzer =
    Fuzz.list entryFuzzer


entryFuzzer : Fuzzer ( String, String )
entryFuzzer =
    Fuzz.tuple ( Fuzz.string, Fuzz.string )

-- from https://github.com/elm-community/result-extra/blob/b59230b2670b19210d8e06ae48463f8fe3432e8f/src/Result/Extra.elm#L222-L248
merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr