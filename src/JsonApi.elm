module JsonApi exposing (ResourceInfo, OneOrManyRelationships, id, links, withId, withLinks, withAttributes, withRelationship, build, relationship, relationships)

{-| JsonApi exposes the `ResourceInfo` type and functions to get and set information
for your resources.


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


{-| The `ResourceInfo` represents a resource. It is passed to your resource decoders, but you can also use it to encode resources to json api.
It contains useful information for decoding and encoding your resource: resource `id`, `links`, `attributes`, `relationships`, ...

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

And how to build it with the `JsonApi` module:

    build "users"
        |> withId "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
        |> withLinks
            (Dict.fromList
                [ ( "self", "http://link-to-user" )
                , ( "profile", "http://link-to-user-profile" )
                ]
            )
        |> withAttributes
            [ ( "firstname", string "John" )
            , ( "lastname", string "Doe" )
            , ( "gender", string "male" )
            ]

-}
type alias ResourceInfo =
    Internal.ResourceInfo


{-| This type is used to represent either or or many relationships in your `ResourceInfo` object.

See `withRelationship` function for more information

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
        build "posts"
            |> withId "post-1"
            |> withLinks (Dict.fromList [ ( "self", "http://url-to-post/1" ) ])

-}
build : String -> ResourceInfo
build type_ =
    Internal.ResourceInfo (Internal.build type_)


{-| Sets the id of the `ResourceInfo` object

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withId "post-1"

-}
withId : String -> ResourceInfo -> ResourceInfo
withId id (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | id = Just id }


{-| Sets the links of the `ResourceInfo` object

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withLinks (Dict.fromList [ ( "self", "http://url-to-post/1" ) ])

-}
withLinks : Dict String String -> ResourceInfo -> ResourceInfo
withLinks links (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | links = links }


{-| Sets the attributes of the `ResourceInfo` object.
This is the payload of your resource.

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withAttributes
                [ ( "firstname", string "John" )
                , ( "lastname", string "Doe" )
                , ( "gender", string "male" )
                ]

-}
withAttributes : List ( String, Value ) -> ResourceInfo -> ResourceInfo
withAttributes attrs (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | attributes = object attrs }


{-| Adds a relationship in the `ResourceInfo` object.
You have to pass it the name of the relationship and a description of the relationship resource (See `relationship` and `relationships`)

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withRelationship "creators" (relationship creator.id (creatorResource creator))

-}
withRelationship : String -> OneOrManyRelationships -> ResourceInfo -> ResourceInfo
withRelationship type_ (Internal.OneOrManyRelationships oneOrMoreRelationships) (Internal.ResourceInfo info) =
    Internal.ResourceInfo (Internal.addRelationship type_ oneOrMoreRelationships info)


{-| Defines a relationship that can then be added to its parent `ResourceInfo`.
It takes the `id` of the resource and the resource.

    creatorResource : Creator -> ResourceInfo
    creatorResource creator =
        build "creator"
            |> withAttributes [ ( "firstname", string creator.firstname ) ]

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withRelationship "creators" (relationship creator.id (creatorResource creator))

-}
relationship : String -> ResourceInfo -> OneOrManyRelationships
relationship id =
    withId id >> Internal.OneRelationship >> Internal.OneOrManyRelationships


{-| Defines a list of relationships that can then be added to a parent `ResourceInfo`.
It takes a `List` of `Tuple`s with the `id` of the resource and the resource.

    commentResource : Comment -> ResourceInfo
    commentResource comment =
        build "comment"
            |> withAttributes [ ( "content", string comment.content ) ]

    myResource : Post -> ResourceInfo
    myResource post =
        build "posts"
            |> withRelationship "comments" (relationships (List.map (\comment -> ( "comment", commentResource comment )) comments))

-}
relationships : List ( String, ResourceInfo ) -> OneOrManyRelationships
relationships =
    List.map (\( id, info ) -> info |> withId id) >> Internal.ManyRelationships >> Internal.OneOrManyRelationships
