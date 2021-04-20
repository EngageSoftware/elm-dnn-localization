module Engage.Localization exposing
    ( Localization
    , decoder
    , localizeString, localizeStringWithDefault, localizeText, localizeTextWithDefault
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
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra


{-| `Dict` for storing Localization text
-}
type alias Localization =
    Dict String String


{-| Localize a key using the given `Localization` dict. If the key is not found, this function will return the key value wrapped in `[ ]`.

    import Dict
    import Engage.Localization exposing (Localization)

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FIRSTNAME.TEXT", "First Name: ") ]

    localizeString "FirstName.Text" { localization = myLocalization }
    --> "First Name: "

    localizeString "LastName.Text" { localization = myLocalization }
    --> "[LastName.Text]"

-}
localizeString : String -> { a | localization : Localization } -> String
localizeString key =
    localizeStringWithDefault ("[" ++ key ++ "]") key


{-| Try to localize a key using the given `Localization` dict, and if the key is not found, return the given default value.

    import Dict
    import Engage.Localization exposing (Localization)

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FIRSTNAME.TEXT", "First Name: ") ]

    localizeStringWithDefault "First Name" "FirstName.Text" { localization = myLocalization }
    --> "First Name: "
    localizeStringWithDefault "Last Name" "LastName.Text" { localization = myLocalization }
    --> "Last Name"

-}
localizeStringWithDefault : String -> String -> { a | localization : Localization } -> String
localizeStringWithDefault default key model =
    let
        keyUppercase =
            String.toUpper key
    in
    Dict.get keyUppercase model.localization
        |> Maybe.Extra.orElse (Dict.get (keyUppercase ++ ".TEXT") model.localization)
        |> Maybe.Extra.orElse (Dict.get (keyUppercase ++ ".ERROR") model.localization)
        |> Maybe.withDefault default


{-| Similar to `localizeText`, but the wrapped the text in `Html.text`

    import Dict
    import Engage.Localization exposing (Localization)
    import Html

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FIRSTNAME.TEXT", "First Name: ") ]

    localizeText "FirstName.Text" { localization = myLocalization }
    --> Html.text "First Name: "
    localizeText "LastName.Text" { localization = myLocalization }
    --> Html.text "[LastName.Text]"

-}
localizeText : String -> { a | localization : Localization } -> Html msg
localizeText key =
    localizeTextWithDefault ("[" ++ key ++ "]") key


{-| Similar to `localizeTextWithDefault`, but the wrapped the text in `Html.text`

    import Dict
    import Engage.Localization exposing (Localization)
    import Html

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FIRSTNAME.TEXT", "First Name: ") ]

    localizeTextWithDefault "First Name" "FirstName.Text" { localization = myLocalization }
    --> Html.text "First Name: "
    localizeTextWithDefault "Last Name" "LastName.Text" { localization = myLocalization }
    --> Html.text "Last Name"

-}
localizeTextWithDefault : String -> String -> { a | localization : Localization } -> Html msg
localizeTextWithDefault default key model =
    Html.text (localizeStringWithDefault default key model)


{-| Decode from JSON values that contains an array of object with `key` and `value` string properties to a `Localization`

    import Dict
    import Engage.Localization exposing (Localization)
    import Json.Decode as Decode
    import Result

    type alias Model =
        { localization : Localization
        }

    """ [ { "key": "FirstName.Text", "value": "First Name:" } ] """
        |> Decode.decodeString decoder
        |> Result.withDefault Dict.empty
        |> Model
        |> localizeString "FirstName"
    --> "First Name:"

-}
decoder : Decoder Localization
decoder =
    Decode.list keyValueDecoder
        |> Decode.map keyValuesToLocalization


keyValuesToLocalization : List ( String, String ) -> Localization
keyValuesToLocalization keyValues =
    keyValues
        |> List.map (\( key, value ) -> ( String.toUpper key, value ))
        |> Dict.fromList


keyValueDecoder : Decoder ( String, String )
keyValueDecoder =
    Decode.map2 (\key value -> ( key, value ))
        (Decode.field "key" Decode.string)
        (Decode.field "value" Decode.string)
