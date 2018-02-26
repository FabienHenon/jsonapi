module JsonApi exposing (ResourceInfo, id, links)

{-| JsonApi exposes the `ResourceInfo` type and functions to get useful information
for your resource decoders.


# Type

@docs ResourceInfo


# Getter functions

@docs id, links

-}

import JsonApi.Internal.ResourceInfo as Internal
import Dict exposing (Dict)


{-| The `ResourceInfo` is passed to your resource decoders. It contains useful information
for decoding and for your resource, like your resource `id` and `links`

_Example of json api <resource:_>

```json
{
    "data": [
        {
            "type": "users",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-user",
                "profile": "http://link-to-user-profile"
            },
            "attributes": {
                "firstname": "John",
                "lastname": "Doe",
                "gender": "male"
            },
            "relationships": {}
        }
    ]
}
```

-}
type alias ResourceInfo =
    Internal.ResourceInfo


{-| Returns the `id` of your resource.

From the json example above, `id` will return `13608770-76dd-47e5-a1c4-4d0d9c2483ad`

-}
id : ResourceInfo -> String
id (Internal.ResourceInfo { id }) =
    id


{-| Returns the `links` of your resource.

From the json example above, `links` will return a `Dict` with this value:

    [ ("self", "http://link-to-user")
    , ("profile", "http://link-to-user-profile")
    ]

-}
links : ResourceInfo -> Dict String String
links (Internal.ResourceInfo { links }) =
    links
