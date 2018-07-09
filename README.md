# elm-dnn-localization

Helpers for working with Engage's DNN localization.

To get started, decode an array of objects into a `Localization` value (which is just an alias for `Dict String String`).

```
import Engage.Localization as Localization exposing (Localization)

localizationJson = """
    [
        { "key": "FirstName.Text", "value": "First Name:" },
        { "key": "LastName.Text", "value": "Last Name:" }
    ]
    """
myLocalization = localizationJson
    |> Json.Decode.decodeString Localization.decode

myModel = {
    localization: Localization
}

labelText = Localization.localizeString "FirstName.Text" myModel
```
