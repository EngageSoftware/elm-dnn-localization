module Engage.Localization exposing
    ( Localization
    , decoder, fromDict
    , localizeString, localizeStringWithDefault, localizeText, localizeTextWithDefault
    )

{-| Helpers for working with DNN Localization for Engage Software team.


# Types

@docs Localization


# Create Localization Dict

@docs decoder, fromDict


# Get localized values

@docs localizeString, localizeStringWithDefault, localizeText, localizeTextWithDefault

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra


{-| `Dict` for storing localized text
-}
type alias Localization =
    Dict String String


{-| Localize a key using the given `Localization` dict.

If the key is not found, this function will return the key value wrapped in `[ ]`.
The key is searched in a case-insensitive manner.
Keys which end in `.Text` may omit that suffix.

    import Dict
    import Engage.Localization as Localization exposing (Localization)

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ] |> Localization.fromDict

    localizeString "FirstName" { localization = myLocalization }
    --> "First Name: "

    localizeString "LastName" { localization = myLocalization }
    --> "[LastName]"

-}
localizeString : String -> { a | localization : Localization } -> String
localizeString key =
    localizeStringWithDefault ("[" ++ key ++ "]") key


{-| Try to localize a key using the given `Localization` dict, and if the key is not found, return the given default value.

The key is searched in a case-insensitive manner.
Keys which end in `.Text` may omit that suffix.

    import Dict
    import Engage.Localization as Localization exposing (Localization)

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ] |> Localization.fromDict

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


{-| Similar to `localizeText`, but with the text wrapped in `Html.text`

    import Dict
    import Engage.Localization as Localization exposing (Localization)
    import Html

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ] |> Localization.fromDict

    localizeText "FirstName.Text" { localization = myLocalization }
    --> Html.text "First Name: "
    localizeText "LastName.Text" { localization = myLocalization }
    --> Html.text "[LastName.Text]"

-}
localizeText : String -> { a | localization : Localization } -> Html msg
localizeText key =
    localizeTextWithDefault ("[" ++ key ++ "]") key


{-| Similar to `localizeTextWithDefault`, but with the text wrapped in `Html.text`

    import Dict
    import Engage.Localization as Localization exposing (Localization)
    import Html

    myLocalization : Localization
    myLocalization = Dict.fromList [ ("FirstName.Text", "First Name: ") ] |> Localization.fromDict

    localizeTextWithDefault "First Name" "FirstName.Text" { localization = myLocalization }
    --> Html.text "First Name: "
    localizeTextWithDefault "Last Name" "LastName.Text" { localization = myLocalization }
    --> Html.text "Last Name"

-}
localizeTextWithDefault : String -> String -> { a | localization : Localization } -> Html msg
localizeTextWithDefault default key model =
    Html.text (localizeStringWithDefault default key model)


{-| Decode from JSON values to a `Localization`.

Supports three formats:

  - an array of object with `key` and `value` string properties

  - an array of object with `Key` and `Value` string properties

  - an object with string properties

```
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

    """ [ { "Key": "LastName.Text", "Value": "Last Name:" } ] """
        |> Decode.decodeString decoder
        |> Result.withDefault Dict.empty
        |> Model
        |> localizeString "lastname"
    --> "Last Name:"

    """ { "FirstName.Text": "Given Name", "FirstName.Help": "Your given name" } """
        |> Decode.decodeString decoder
        |> Result.withDefault Dict.empty
        |> Model
        |> localizeString "FirstName.Help"
    --> "Your given name"
```

-}
decoder : Decoder Localization
decoder =
    let
        keyValueDecoder : Decoder (List ( String, String ))
        keyValueDecoder =
            Decode.map2 (\key value -> ( key, value ))
                (Decode.oneOf [ Decode.field "key" Decode.string, Decode.field "Key" Decode.string ])
                (Decode.oneOf [ Decode.field "value" Decode.string, Decode.field "Value" Decode.string ])
                |> Decode.list

        objectDecoder : Decoder (List ( String, String ))
        objectDecoder =
            Decode.dict Decode.string
                |> Decode.map Dict.toList
    in
    Decode.oneOf [ keyValueDecoder, objectDecoder ]
        |> Decode.map fromKeyValuePairs


{-| Convert a dictionary to a `Localization`.

This normalizes the data in the dictionary (i.e. it is not the same as just using a `Dict String String` directly).

    import Dict
    import Engage.Localization as Localization

    type alias Model =
        { localization : Localization
        }

    Dict.fromList [ ("FirstName.Text", "First Name:") ]
        |> Localization.fromDict
        |> Model
        |> Localization.localizeString "FirstName"
    --> "First Name:"

-}
fromDict : Dict String String -> Localization
fromDict dict =
    dict
        |> Dict.toList
        |> fromKeyValuePairs


fromKeyValuePairs : List ( String, String ) -> Localization
fromKeyValuePairs pairs =
    pairs
        |> List.map (\( key, value ) -> ( String.toUpper key, value ))
        |> Dict.fromList
