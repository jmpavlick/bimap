# Bimap: Simple bidirectional mapping between Strings and custom types

This package provides a type-safe data structure and helper functions for mapping between a `String` and a custom type, whose values don't take any parameters.

Think of it as sugar around the general idea of an "enum" in other languages.

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
        |> Bimap.vairant "Yes" Yes
        |> Bimap.variant "No" No
        |> Bimap.build


Bimap.toString Yes --> "Yes"

Bimap.fromString "No" --> Just No

Bimap.fromString "Maybe" --> Nothing
```

Inspired by [miniBill/elm-codec](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/). Useful whenever you need to map between `String`s and custom types, but JSON values aren't involved.