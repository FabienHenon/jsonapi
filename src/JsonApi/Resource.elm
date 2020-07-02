module JsonApi.Resource exposing
    ( Resource, OneOrManyRelationships, OneOrMoreRelationshipData(..), RelationshipData, RelationshipDesc
    , build, fromResource
    , id, links, resType, getRelationshipsDesc
    , withId, withLinks, withAttributes, withRelationship
    , relationship, relationships
    )

{-| JsonApi.Resource exposes the `Resource` type and functions to get and set information
for your resources.


# Type

@docs Resource, OneOrManyRelationships, OneOrMoreRelationshipData, RelationshipData, RelationshipDesc


# New resource

@docs build, fromResource


# Getter functions

@docs id, links, resType, getRelationshipsDesc


# Setter functions

@docs withId, withLinks, withAttributes, withRelationship


# Relationships

@docs relationship, relationships

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value, object)
import JsonApi.Internal.ResourceInfo as Internal


{-| The `Resource` represents a resource. It is passed to your resource decoders, but you can also use it to encode resources to json api, via a `Document`.
It contains useful information for decoding and encoding your resource: resource `id`, `links`, `attributes`, `relationships`, ...

\_Example of json api <resource:_>

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

And how to build it with the `JsonApi.Resource` module:

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
type alias Resource =
    Internal.ResourceInfo


{-| This type is used to represent either one or many relationships in your `Resource` object.

See `withRelationship` function for more information

-}
type alias OneOrManyRelationships =
    Internal.OneOrManyRelationships


{-| Relationship's data object. It's a variant type because in a relationship
the `data` field can either: be null, have one relationship, have many relationships.

_See `getRelationshipsDesc` for more information_

-}
type OneOrMoreRelationshipData
    = NoRelationship
    | One RelationshipData
    | Many (List RelationshipData)


{-| Description of the `data` object in a `relationship` object.
Contains the `id` of the relationship and it's `type`

_See `getRelationshipsDesc` for more information_

-}
type alias RelationshipData =
    { id : String
    , type_ : String
    }


{-| The description of a relationship in the `relationships` field of the jsonapi object.

For instance (in jsonapi):

    "relationships": {
        "creator": {
            "data": {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
            },
            "links": {
                "related": "http://link-to-creator/1"
            }
        }
    }

This type contains the links defined in the relationship and the `data` object of
the relationship

_See `getRelationshipsDesc` for more information_

-}
type alias RelationshipDesc =
    { data : OneOrMoreRelationshipData
    , links : Dict String String
    }


{-| Returns the `id` of your resource.

From the json example above, `id` will return `13608770-76dd-47e5-a1c4-4d0d9c2483ad`

-}
id : Resource -> String
id (Internal.ResourceInfo res) =
    res.id |> Maybe.withDefault ""


{-| Returns the `type` of your resource.

From the json example above, `type_` will return `users`

-}
resType : Resource -> String
resType (Internal.ResourceInfo { type_ }) =
    type_


{-| Returns the `links` of your resource.

From the json example above, `links` will return a `Dict` with this value:

    [ ( "self", "http://link-to-user" )
    , ( "profile", "http://link-to-user-profile" )
    ]

-}
links : Resource -> Dict String String
links (Internal.ResourceInfo res) =
    res.links


{-| Returns the relationships description of your resource.

**This is not a way to decode a relationship, only a quick way to get the relationship ids and types**

    type alias Post =
        { id : String
        , links : Dict String String
        , title : String
        , content : String
        , relationships : Dict String JsonApi.Resource.RelationshipDesc
        }

    decoder : Resource -> Decoder Post
    decoder resourceInfo =
        map5 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (succeed (JsonApi.Resource.links resourceInfo))
            (field "title" string)
            (field "content" string)
            (succeed (JsonApi.Resource.getRelationshipsDesc resourceInfo))

Say the relationships for this post are:

    "relationships": {
        "creator": {
            "data": {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
            },
            "links": {
                "related": "http://link-to-creator/1"
            }
        },
        "comments": {
            "links": {},
            "data": [
                {
                    "type": "comment",
                    "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                },
                {
                    "type": "comment",
                    "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                }
            ]
        }
    }

Then you will get these information:

    Dict.get "creator" post.relationships

    Just
        { links = [ ( "related", "http://link-to-creator/1" ) ]
        , data =
            JsonApi.Resource.One
                { id = "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                , type_ = "creators"
                }
        }

    Dict.get "comments" post.relationships

    Just
        { links = []
        , data =
            JsonApi.Resource.Many
                [ { id = "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                  , type_ = "comment"
                  }
                , { id = "cb0759b0-03ab-4291-b067-84a9017fea6f"
                  , type_ = "comment"
                  }
                ]
        }

-}
getRelationshipsDesc : Resource -> Dict String RelationshipDesc
getRelationshipsDesc (Internal.ResourceInfo res) =
    res.relationships
        |> Dict.map
            (\k v ->
                { links = v.links
                , data =
                    case v.data of
                        Internal.NoRelationship ->
                            NoRelationship

                        Internal.One e ->
                            One e

                        Internal.Many e ->
                            Many e
                }
            )


{-| Builds a new `Resource` with the specified type name

You can build your resources like this:

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withId "post-1"
            |> withLinks (Dict.fromList [ ( "self", "http://url-to-post/1" ) ])

-}
build : String -> Resource
build type_ =
    Internal.ResourceInfo (Internal.build type_)


{-| Builds a new `Resource` from the specified `Resource` and with the specified type name

You can build your resources like this:

    myResource : Resource -> Resource
    myResource resource =
        fromResource "posts" resource

-}
fromResource : String -> Resource -> Resource
fromResource type_ (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | type_ = type_ }


{-| Sets the id of the `Resource` object

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withId "post-1"

-}
withId : String -> Resource -> Resource
withId id_ (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | id = Just id_ }


{-| Sets the links of the `Resource` object

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withLinks (Dict.fromList [ ( "self", "http://url-to-post/1" ) ])

-}
withLinks : Dict String String -> Resource -> Resource
withLinks links_ (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | links = links_ }


{-| Sets the attributes of the `Resource` object.
This is the payload of your resource.

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withAttributes
                [ ( "firstname", string "John" )
                , ( "lastname", string "Doe" )
                , ( "gender", string "male" )
                ]

-}
withAttributes : List ( String, Value ) -> Resource -> Resource
withAttributes attrs (Internal.ResourceInfo info) =
    Internal.ResourceInfo { info | attributes = object attrs }


{-| Adds a relationship in the `Resource` object.
You have to pass it the name of the relationship and a description of the relationship resource (See `relationship` and `relationships`)

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withRelationship "creators" (relationship creator.id (creatorResource creator))

-}
withRelationship : String -> OneOrManyRelationships -> Resource -> Resource
withRelationship type_ (Internal.OneOrManyRelationships oneOrMoreRelationships) (Internal.ResourceInfo info) =
    Internal.ResourceInfo (Internal.addRelationship type_ oneOrMoreRelationships info)


{-| Defines a relationship that can then be added to its parent `Resource`.
It takes the `id` of the resource and the resource.

    creatorResource : Creator -> Resource
    creatorResource creator =
        build "creator"
            |> withAttributes [ ( "firstname", string creator.firstname ) ]

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withRelationship "creators" (relationship creator.id (creatorResource creator))

-}
relationship : String -> Resource -> OneOrManyRelationships
relationship id_ =
    withId id_ >> Internal.OneRelationship >> Internal.OneOrManyRelationships


{-| Defines a list of relationships that can then be added to a parent `Resource`.
It takes a `List` of `Tuple`s with the `id` of the resource and the resource.

    commentResource : Comment -> Resource
    commentResource comment =
        build "comment"
            |> withAttributes [ ( "content", string comment.content ) ]

    myResource : Post -> Resource
    myResource post =
        build "posts"
            |> withRelationship "comments" (relationships (List.map (\comment -> ( "comment", commentResource comment )) comments))

-}
relationships : List ( String, Resource ) -> OneOrManyRelationships
relationships =
    List.map (\( id_, info ) -> info |> withId id_) >> Internal.ManyRelationships >> Internal.OneOrManyRelationships
