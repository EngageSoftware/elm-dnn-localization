module Engage.Localization
    exposing
        ( Localization
        , decoder
        , localizeString
        , localizeStringWithDefault
        , localizeText
        , localizeTextWithDefault
        )

{-| Helpers for working with DNN Localization for Engage Software team.


# Types

@docs Localization


# Decoders

@docs decoder


# Localization functions

@docs localizeString, localizeStringWithDefault, localizeText, localizeTextWithDefault

-}

import Dict exposing (Dict)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode exposing (decode, required)
import Maybe.Extra exposing (or)
import String exposing (toUpper)


type alias KeyValue =
    { key : String, value : String }


{-| `Dict` for storing Localization text
-}
type alias Localization =
    Dict String String


{-| Localize a key using the given `Localization` dict. If the key is not found, this function will return the key value wrapped in `[ ]`.

    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ]

    localizeString "FirstName.Text" { localization = myLocalization } == "First Name:"
    localizeString "LastName.Text" { localization = myLocalization } == "[LastName.Text]"

-}
localizeString : String -> { a | localization : Localization } -> String
localizeString key =
    localizeStringWithDefault ("[" ++ key ++ "]") key


{-| Try to localize a key using the given `Localization` dict, and if the key is not found, return the given default value.

    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ]

    localizeStringWithDefault "First Name" "FirstName.Text" { localization = myLocalization } == "First Name:"
    localizeStringWithDefault "Last Name" "LastName.Text" { localization = myLocalization } == "Last Name"

-}
localizeStringWithDefault : String -> String -> { a | localization : Localization } -> String
localizeStringWithDefault default key model =
    let
        keyUppercase =
            toUpper key
    in
    Dict.get keyUppercase model.localization
        |> flip or (Dict.get (keyUppercase ++ ".TEXT") model.localization)
        |> flip or (Dict.get (keyUppercase ++ ".ERROR") model.localization)
        |> Maybe.withDefault default


{-| Similar to `localizeText`, but the wrapped the text in `Html.text`

    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ]

    localizeText "FirstName.Text" { localization = myLocalization } == Html.text "First Name:"
    localizeText "LastName.Text" { localization = myLocalization } == Html.text "[LastName.Text]"

-}
localizeText : String -> { a | localization : Localization } -> Html msg
localizeText key =
    localizeTextWithDefault ("[" ++ key ++ "]") key


{-| Similar to `localizeTextWithDefault`, but the wrapped the text in `Html.text`

    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ]

    localizeTextWithDefault "First Name" "FirstName.Text" { localization = myLocalization } == Html.text "First Name:"
    localizeTextWithDefault "Last Name" "LastName.Text" { localization = myLocalization } == Html.text "Last Name"

-}
localizeTextWithDefault : String -> String -> { a | localization : Localization } -> Html msg
localizeTextWithDefault default key model =
    text (localizeStringWithDefault default key model)


keyValuesToLocalization : List KeyValue -> Localization
keyValuesToLocalization keyValues =
    keyValues
        |> List.map (\keyValue -> ( toUpper keyValue.key, keyValue.value ))
        |> Dict.fromList


toLocalization : { options | localization : List KeyValue } -> { options | localization : Localization }
toLocalization jsOptions =
    let
        keyValues =
            jsOptions.localization
    in
    { jsOptions | localization = keyValuesToLocalization keyValues }


keyValueDecoder : Decoder KeyValue
keyValueDecoder =
    decode KeyValue
        |> required "key" Decode.string
        |> required "value" Decode.string


{-| Decode from JSON values that contains an array of object with `key` and `value` string properties to a `Localization`
-}
decoder : Decoder Localization
decoder =
    Decode.list keyValueDecoder
        |> Decode.map keyValuesToLocalization
