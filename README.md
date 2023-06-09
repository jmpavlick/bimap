# Bimap: Simple bidirectional mapping between Strings and custom types

This package provides a type-safe data structure and helper functions for mapping between a `String` and a custom type, whose values don't take any parameters.

Think of it as sugar around the general idea of an "enum" in other languages.

Supports:

* Mapping between custom types and `String`s
* Comparison operators (for sorting)
* JSON encoders and decoders for custom type values

```
import Bimap exposing (Bimap)

type Response
    = Yes
    | No

bimap = Bimap Response
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


Bimap.toString bimap Yes --> "Yes"

Bimap.fromString bimap "No" --> Just No

Bimap.fromString bimap "Maybe" --> Nothing
```

Inspired by [miniBill/elm-codec](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/). Useful whenever you need to map between `String`s and custom types, but JSON values aren't involved.