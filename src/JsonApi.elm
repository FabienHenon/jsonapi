module JsonApi exposing (ResourceInfo, OneOrManyRelationships, id, links, withId, withLinks, withAttributes, withRelationship, build, relationship, relationships)

{-| JsonApi exposes the `ResourceInfo` type and functions to get useful information
for your resource decoders.


# Type

@docs ResourceInfo, OneOrManyRelationships


# New resource

@docs build


# Getter functions

@docs id, links


# Setter functions

@docs withId, withLinks, withAttributes, withRelationship


# Relationships

@docs relationship, relationships

-}

import JsonApi.Internal.ResourceInfo as Internal
import Dict exposing (Dict)
import Json.Encode exposing (Value, object)


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


{-| TODO
-}
type alias OneOrManyRelationships =
    Internal.OneOrManyRelationships


{-| Returns the `id` of your resource.

From the json example above, `id` will return `13608770-76dd-47e5-a1c4-4d0d9c2483ad`

-}
id : ResourceInfo -> String
id (Internal.ResourceInfo { id }) =
    id |> Maybe.withDefault ""


{-| Returns the `links` of your resource.

From the json example above, `links` will return a `Dict` with this value:

    [ ("self", "http://link-to-user")
    , ("profile", "http://link-to-user-profile")
    ]

-}
links : ResourceInfo -> Dict String String
links (Internal.ResourceInfo { links }) =
    links


{-| Builds a new `ResourceInfo` with the specified type name

You can build your resources like this:

    myResource : Post -> ResourceInfo
    myResource post =
        JsonApi.build "posts"
            |> JsonApi.withId "post-1"
            |> JsonApi.withLinks (Dict.fromList [ ( "self", "http://url-to-post/1" ) ])

-}
build : String -> ResourceInfo
build type_ =
    Internal.ResourceInfo (Internal.build type_)


{-| Sets the id of the `ResourceInfo` object
-}
withId : String -> ResourceInfo -> ResourceInfo
withId id (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | id = Just id }


{-| Sets the links of the `ResourceInfo` object
-}
withLinks : Dict String String -> ResourceInfo -> ResourceInfo
withLinks links (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | links = links }


{-| Sets the attributes of the `ResourceInfo` object
-}
withAttributes : List ( String, Value ) -> ResourceInfo -> ResourceInfo
withAttributes attrs (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | attributes = object attrs }


{-| TODO
-}
withRelationship : String -> OneOrManyRelationships -> ResourceInfo -> ResourceInfo
withRelationship type_ (Internal.OneOrManyRelationships oneOrMoreRelationships) (Internal.ResourceInfo info) =
    Internal.ResourceInfo (Internal.addRelationship type_ oneOrMoreRelationships info)


{-| TODO
-}
relationship : String -> ResourceInfo -> OneOrManyRelationships
relationship id =
    withId id >> Internal.OneRelationship >> Internal.OneOrManyRelationships


{-| TODO
-}
relationships : List ( String, ResourceInfo ) -> OneOrManyRelationships
relationships =
    List.map (\( id, info ) -> info |> withId id) >> Internal.ManyRelationships >> Internal.OneOrManyRelationships
