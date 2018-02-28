module JsonApi.Decode exposing (resources, resource, relationship, relationships)

{-| Provides functions to decode json api resources and their relationships

*Example json:*

```json
{
    "data": [
        {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
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
        },
        {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ae",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "Second post",
                "content": "Second post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://lnk-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ac"
                        }
                    ]
                }
            }
        }
    ],
    "included": [
        {
            "type": "creators",
            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
            "attributes": {
                "firstname": "John",
                "lastname": "Doe"
            },
            "links": {
                "self": "http://link-to-creator/1"
            },
            "relationships": {}
        },
        {
            "type": "comment",
            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ac",
            "attributes": {
                "content": "Comment 1 content",
                "email": "john@doe.com"
            },
            "links": {
                "self": "http://link-to-comment/1"
            },
            "relationships": {}
        },
        {
            "type": "comment",
            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
            "attributes": {
                "content": "Comment 2 content",
                "email": "john@doe.com"
            },
            "links": {
                "self": "http://link-to-comment/2"
            },
            "relationships": {}
        },
        {
            "type": "comment",
            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
            "attributes": {
                "content": "Comment 3 content",
                "email": "john@doe.com"
            },
            "links": {
                "self": "http://link-to-comment/3"
            },
            "relationships": {}
        }
    ]
}
```


# Decoders

@docs resources, resource, relationship, relationships

-}

import JsonApi.Internal.ResourceInfo as Internal
import JsonApi exposing (ResourceInfo)
import Json.Decode exposing (Value, Decoder, list, field, map, andThen, succeed, fail, string, dict, oneOf, value, decodeValue)
import Json.Decode.Extra exposing ((|:))
import List.Extra
import Dict exposing (Dict)


{-| Decode a relationship from your json api resources.

You pass it the type of the relationship resource (`"creator"` in our example above), the `ResourceInfo`
passed to your resources `Decoder` and the relationship decoder. It will return a
new `Decoder` representing the relationship.

Here is an example of resource `Decoder` with a relationship:

    type alias Post =
        { id : String
        , title : String
        , content : String
        , creator : Creator
        }

    type alias Creator =
        { id : String
        , firstname : String
        , lastname : String
        }

    creatorDecoder : ResourceInfo -> Decoder Creator
    creatorDecoder resourceInfo =
        map3 Creator
            (succeed (JsonApi.id resourceInfo))
            (field "firstname" string)
            (field "lastname" string)

    postDecoder : ResourceInfo -> Decoder Post
    postDecoder resourceInfo =
        map4 Post
            (succeed (JsonApi.id resourceInfo))
            (field "title" string)
            (field "content" string)
            (relationship "creator" resourceInfo creatorDecoder)

    -- Decoder for our posts and its creator from json api
    resources "posts" postDecoder

-}
relationship : String -> ResourceInfo -> (ResourceInfo -> Decoder a) -> Decoder a
relationship type_ (Internal.ResourceInfo info) decoder =
    info.relationships
        |> Dict.get type_
        |> Maybe.map .data
        |> Maybe.andThen (findRelationship info.included)
        |> Maybe.map (decodeRelationship decoder)
        |> Maybe.withDefault (fail "Relationship not found")


{-| Decode a list of relationships from your json api resources.

You pass it the type of the relationship resource (`"comments"` in our example above), the `ResourceInfo`
passed to your resources `Decoder` and the relationship decoder. It will return a
new `Decoder` representing the `List` of relationships.

Here is an example of resource `Decoder` with a list of relationships:

    type alias Post =
        { id : String
        , title : String
        , content : String
        , comments : List Comment
        }

    type alias Comment =
        { id : String
        , content : String
        , email : String
        }

    commentDecoder : ResourceInfo -> Decoder Comment
    commentDecoder resourceInfo =
        map3 Comment
            (succeed (JsonApi.id resourceInfo))
            (field "content" string)
            (field "email" string)

    postDecoder : ResourceInfo -> Decoder Post
    postDecoder resourceInfo =
        map4 Post
            (succeed (JsonApi.id resourceInfo))
            (field "title" string)
            (field "content" string)
            (relationships "comments" resourceInfo commentDecoder)

    -- Decoder for our posts and its creator from json api
    resources "posts" postDecoder

-}
relationships : String -> ResourceInfo -> (ResourceInfo -> Decoder a) -> Decoder (List a)
relationships type_ (Internal.ResourceInfo info) decoder =
    info.relationships
        |> Dict.get type_
        |> Maybe.map (.data)
        |> Maybe.andThen (findRelationships info.included)
        |> Maybe.map (decodeRelationships decoder)
        |> Maybe.withDefault (fail "Relationships not found")


{-| Decode resources from the json api content.

You pass it the type of the resources (`"posts"` in our example above) and the resource decoder and it will return a
new `Decoder` representing a `List` of your resources.

Here is an example of resource `Decoder`:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    postDecoder : ResourceInfo -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.id resourceInfo))
            (field "title" string)
            (field "content" string)

    -- Decoder for our posts from json api
    resources "posts" postDecoder

-}
resources : String -> (ResourceInfo -> Decoder a) -> Decoder (List a)
resources type_ decoder =
    field "included" includedDecoder
        |> andThen (resourcesDataDecoder type_ decoder)


{-| Decode only one resource from the json api content.

You pass it the type of the resource (`"posts"` in our example above) and the resource decoder and it will return a
new `Decoder` representing your resource.

**(The json `data` attribute is an object and not a list)**

Here is an example of resource `Decoder`:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    postDecoder : ResourceInfo -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.id resourceInfo))
            (field "title" string)
            (field "content" string)

    -- Decoder for our post from json api
    resource "posts" postDecoder

-}
resource : String -> (ResourceInfo -> Decoder a) -> Decoder a
resource type_ decoder =
    field "included" includedDecoder
        |> andThen (resourceDataDecoder type_ decoder)



-- LOGIC


resourcesDataDecoder : String -> (ResourceInfo -> Decoder a) -> List ResourceInfo -> Decoder (List a)
resourcesDataDecoder type_ decoder included =
    field "data" (list (dataDecoder type_ decoder included))
        |> map (List.filterMap identity)


resourceDataDecoder : String -> (ResourceInfo -> Decoder a) -> List ResourceInfo -> Decoder a
resourceDataDecoder type_ decoder included =
    field "data" (dataDecoder type_ decoder included)
        |> andThen (Maybe.map succeed >> Maybe.withDefault (fail "data type not found"))


dataDecoder : String -> (ResourceInfo -> Decoder a) -> List ResourceInfo -> Decoder (Maybe a)
dataDecoder type_ decoder =
    resourceInfoInternalDecoder >> andThen (filterDataType type_ decoder)


filterDataType : String -> (ResourceInfo -> Decoder a) -> Internal.ResourceInfoInternal -> Decoder (Maybe a)
filterDataType dataType decoder info =
    if dataType == info.type_ then
        field "attributes" (decoder (Internal.ResourceInfo info)) |> map Just
    else
        succeed Nothing


resourceInfoInternalDecoder : List ResourceInfo -> Decoder Internal.ResourceInfoInternal
resourceInfoInternalDecoder included =
    succeed (Internal.ResourceInfoInternal included)
        |: (field "id" string |> map Just)
        |: (oneOf [ field "links" linksDecoder, succeed Dict.empty ])
        |: (field "type" string)
        |: (oneOf [ field "relationships" resourceRelationshipsDecoder, succeed Dict.empty ])
        |: (field "attributes" value)


linksDecoder : Decoder (Dict String String)
linksDecoder =
    dict string


resourceRelationshipsDecoder : Decoder (Dict String Internal.Relationship)
resourceRelationshipsDecoder =
    dict resourceRelationshipDecoder


resourceRelationshipDecoder : Decoder Internal.Relationship
resourceRelationshipDecoder =
    succeed Internal.Relationship
        |: (field "data" resourceOneOrMoreRelationshipDataDecoder)
        |: (oneOf [ field "links" linksDecoder, succeed Dict.empty ])


resourceOneOrMoreRelationshipDataDecoder : Decoder Internal.OneOrMoreRelationshipData
resourceOneOrMoreRelationshipDataDecoder =
    oneOf
        [ resourceRelationshipDataDecoder |> map Internal.One
        , list resourceRelationshipDataDecoder |> map Internal.Many
        ]


resourceRelationshipDataDecoder : Decoder Internal.RelationshipData
resourceRelationshipDataDecoder =
    succeed Internal.RelationshipData
        |: (field "id" string)
        |: (field "type" string)


findRelationship : List ResourceInfo -> Internal.OneOrMoreRelationshipData -> Maybe ResourceInfo
findRelationship included oneOrMoreRelationshipData =
    case oneOrMoreRelationshipData of
        Internal.One relationshipData ->
            List.Extra.find (isGoodRelationship relationshipData) included

        Internal.Many _ ->
            Nothing


findRelationships : List ResourceInfo -> Internal.OneOrMoreRelationshipData -> Maybe (List ResourceInfo)
findRelationships included oneOrMoreRelationshipData =
    case oneOrMoreRelationshipData of
        Internal.One _ ->
            Nothing

        Internal.Many listRelationshipData ->
            listRelationshipData
                |> List.foldl
                    (\relationshipData res ->
                        case res of
                            Nothing ->
                                Nothing

                            Just list ->
                                List.Extra.find (isGoodRelationship relationshipData) included
                                    |> Maybe.map (flip (::) list)
                    )
                    (Just [])
                |> Maybe.map List.reverse


isGoodRelationship : Internal.RelationshipData -> ResourceInfo -> Bool
isGoodRelationship relationshipData (Internal.ResourceInfo { id, type_ }) =
    id == (Just relationshipData.id) && type_ == relationshipData.type_


includedDecoder : Decoder (List ResourceInfo)
includedDecoder =
    list (resourceInfoInternalDecoder [])
        |> map (List.map Internal.ResourceInfo)


decodeRelationship : (ResourceInfo -> Decoder a) -> ResourceInfo -> Decoder a
decodeRelationship decoder (Internal.ResourceInfo info) =
    case decodeValue (decoder (Internal.ResourceInfo info)) info.attributes of
        Ok res ->
            succeed res

        Err err ->
            fail err


decodeRelationships : (ResourceInfo -> Decoder a) -> List ResourceInfo -> Decoder (List a)
decodeRelationships decoder =
    List.foldl
        (\(Internal.ResourceInfo info) res ->
            case res of
                Nothing ->
                    Nothing

                Just list ->
                    decodeValue (decoder (Internal.ResourceInfo info)) info.attributes
                        |> Result.toMaybe
                        |> Maybe.map (flip (::) list)
        )
        (Just [])
        >> Maybe.map List.reverse
        >> Maybe.map succeed
        >> Maybe.withDefault (fail "decode relationships failed")
