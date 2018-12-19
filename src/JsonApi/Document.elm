module JsonApi.Document exposing
    ( Document, NoMeta, NoData
    , jsonApiVersion, meta, resource
    )

{-| Provides a type alias and functions to handle decoded Json API documents.


# Types

@docs Document, NoMeta, NoData


# Getter functions

@docs jsonApiVersion, meta, resource

-}

import Dict exposing (Dict)
import Json.Decode exposing (Value)
import JsonApi.Internal.Document as Internal


{-| `NoMeta` is a type of `meta` object from a Json API `Document` that means
there is no meta to be decoded in this `Document`.
-}
type alias NoMeta =
    Internal.NoMeta


{-| `NoData` is a type of resource that means there is no `data` object or array in
the json api document.
-}
type alias NoData =
    Internal.NoData


{-| The `Document` represents the global Json API document.
This version of the `Document` is retrieved via `JsonApi.Decode.` functions.

It allows you to retrieve information about the decoded `Document` and the decoded
`Resource`(s)

_Example of json api document:_

```json
{
    "data": [
        // resources ...
    ],
    "meta": {
        "redirect": true
    },
    "jsonapi": {
        "version": "1.0"
    }
}
```

With a resource and a meta with the following type aliases:

    type alias Post =
        { id : String
        , name : String
        }

    type alias Meta =
        { redirect : Bool
        }

The decoded `Document` will have the following type `Document Meta Post` if it contains
only one resource, or `Document Meta (List Post)` if it contains several resources.

-}
type alias Document meta data =
    Internal.Document meta data


{-| Retrieves the Json API version.

_The default version is 1.0_

-}
jsonApiVersion : Document meta data -> String
jsonApiVersion (Internal.Document doc) =
    doc.jsonApiVersion


{-| Retrieves the `meta` object of the `Document` if it exists or a `NoMeta` object.
-}
meta : Document meta data -> meta
meta (Internal.Document doc) =
    doc.meta


{-| Retrieves the resource or the `List` of resources in this `Document`.
-}
resource : Document meta data -> data
resource (Internal.Document doc) =
    doc.data
