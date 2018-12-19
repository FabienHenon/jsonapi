module JsonApi.Decode exposing
    ( resources, resource, resourcesWithMeta, resourceWithMeta, relationship, relationships
    , meta
    )

{-| Provides functions to decode json api document with their resources and their relationships

_Example json:_

```json
{
    "meta": {
        "redirect": true
    },
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

@docs resources, resource, resourcesWithMeta, resourceWithMeta, relationship, relationships

-}

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value, andThen, at, decodeValue, dict, errorToString, fail, field, list, map, map3, maybe, oneOf, string, succeed, value)
import Json.Decode.Extra exposing (andMap)
import JsonApi.Document as Document
import JsonApi.Internal.Document as DocInternal
import JsonApi.Internal.ResourceInfo as Internal
import JsonApi.Resource exposing (Resource)
import List.Extra


defaultJsonApiVersion : String
defaultJsonApiVersion =
    "1.0"


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

    creatorDecoder : Resource -> Decoder Creator
    creatorDecoder resourceInfo =
        map3 Creator
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "firstname" string)
            (field "lastname" string)

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map4 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)
            (relationship "creator" resourceInfo creatorDecoder)

    -- Decoder for our posts and its creator from json api
    resources "posts" postDecoder

-}
relationship : String -> Resource -> (Resource -> Decoder a) -> Decoder a
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

    commentDecoder : Resource -> Decoder Comment
    commentDecoder resourceInfo =
        map3 Comment
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "content" string)
            (field "email" string)

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map4 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)
            (relationships "comments" resourceInfo commentDecoder)

    -- Decoder for our posts and its creator from json api
    resources "posts" postDecoder

-}
relationships : String -> Resource -> (Resource -> Decoder a) -> Decoder (List a)
relationships type_ (Internal.ResourceInfo info) decoder =
    info.relationships
        |> Dict.get type_
        |> Maybe.map .data
        |> Maybe.andThen (findRelationships info.included)
        |> Maybe.map (decodeRelationships decoder)
        |> Maybe.withDefault (fail "Relationships not found")


{-| Decode a document and its resources from the json api content.

You pass it the type of the resources (`"posts"` in our example above) and the resource decoder and it will return a
new `Decoder` representing a `Document` with `NoMeta` and a `List` of your resources.

Here is an example of resource `Decoder`:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)

    -- Decoder for our posts from json api
    resources "posts" postDecoder

-}
resources : String -> (Resource -> Decoder a) -> Decoder (Document.Document Document.NoMeta (List a))
resources type_ decoder =
    map3 DocInternal.DocumentInternal
        jsonApiVersionDecoder
        (succeed DocInternal.NoMeta)
        (resources_ type_ decoder)
        |> map DocInternal.Document


{-| Decode a document, its meta object and its resources from the json api content.

You pass it the type of the resources (`"posts"` in our example above), the resource decoder and the meta decoder and it will return a
new `Decoder` representing a `Document` with your meta object and a `List` of your resources.

Here is an example of resource `Decoder` with meta:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    type alias Meta =
        { redirect : Bool
        }

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)

    metaDecoder : Decoder Meta
    metaDecoder =
        map Meta
            (field "redirect" bool)

    -- Decoder for our posts from json api
    resourcesWithMeta "posts" postDecoder metaDecoder

-}
resourcesWithMeta : String -> (Resource -> Decoder a) -> Decoder meta -> Decoder (Document.Document meta (List a))
resourcesWithMeta type_ decoder metaDecoder_ =
    map3 DocInternal.DocumentInternal
        jsonApiVersionDecoder
        (metaDecoder metaDecoder_)
        (resources_ type_ decoder)
        |> map DocInternal.Document


resources_ : String -> (Resource -> Decoder a) -> Decoder (List a)
resources_ type_ decoder =
    oneOf [ field "included" includedDecoder, succeed [] ]
        |> andThen (resourcesDataDecoder type_ decoder)


{-| Decode a document with only one resource from the json api content.

You pass it the type of the resource (`"posts"` in our example above) and the resource decoder and it will return a
new `Decoder` representing a `Document` with `NoMeta` and your resource.

**(The json `data` attribute is an object and not a list)**

Here is an example of resource `Decoder`:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)

    -- Decoder for our post from json api
    resource "posts" postDecoder

-}
resource : String -> (Resource -> Decoder a) -> Decoder (Document.Document Document.NoMeta a)
resource type_ decoder =
    map3 DocInternal.DocumentInternal
        jsonApiVersionDecoder
        (succeed DocInternal.NoMeta)
        (resource_ type_ decoder)
        |> map DocInternal.Document


{-| Decode a document, its meta object and only one resource from the json api content.

You pass it the type of the resources (`"posts"` in our example above), the resource decoder and the meta decoder and it will return a
new `Decoder` representing a `Document` with your meta object and your resource.

**(The json `data` attribute is an object and not a list)**

Here is an example of resource `Decoder` with meta:

    type alias Post =
        { id : String
        , title : String
        , content : String
        }

    type alias Meta =
        { redirect : Bool
        }

    postDecoder : Resource -> Decoder Post
    postDecoder resourceInfo =
        map3 Post
            (succeed (JsonApi.Resource.id resourceInfo))
            (field "title" string)
            (field "content" string)

    metaDecoder : Decoder Meta
    metaDecoder =
        map Meta
            (field "redirect" bool)

    -- Decoder for our post from json api
    resourceWithMeta "posts" postDecoder metaDecoder

-}
resourceWithMeta : String -> (Resource -> Decoder a) -> Decoder meta -> Decoder (Document.Document meta a)
resourceWithMeta type_ decoder metaDecoder_ =
    map3 DocInternal.DocumentInternal
        jsonApiVersionDecoder
        (metaDecoder metaDecoder_)
        (resource_ type_ decoder)
        |> map DocInternal.Document


resource_ : String -> (Resource -> Decoder a) -> Decoder a
resource_ type_ decoder =
    oneOf [ field "included" includedDecoder, succeed [] ]
        |> andThen (resourceDataDecoder type_ decoder)


{-| Decode a document, its meta object and no resource from the json api content.

You pass it the meta decoder and it will return a
new `Decoder` representing a `Document` with your meta object and no resource.

**(No `data` property is decoded from the json api document)**

Here is an example of document `Decoder` with only meta:

    type alias Meta =
        { redirect : Bool
        }

    metaDecoder : Decoder Meta
    metaDecoder =
        map Meta
            (field "redirect" bool)

    -- Decoder for our meta object from json api
    meta metaDecoder

-}
meta : Decoder meta -> Decoder (Document.Document meta Document.NoData)
meta metaDecoder_ =
    map3 DocInternal.DocumentInternal
        jsonApiVersionDecoder
        (metaDecoder metaDecoder_)
        (succeed DocInternal.NoData)
        |> map DocInternal.Document



-- LOGIC


metaDecoder : Decoder meta -> Decoder meta
metaDecoder metaDecoder_ =
    field "meta" metaDecoder_


jsonApiVersionDecoder : Decoder String
jsonApiVersionDecoder =
    oneOf [ at [ "jsonapi", "version" ] string, succeed defaultJsonApiVersion ]


resourcesDataDecoder : String -> (Resource -> Decoder a) -> List Resource -> Decoder (List a)
resourcesDataDecoder type_ decoder included =
    field "data" (list (dataDecoder type_ decoder included))
        |> map (List.filterMap identity)


resourceDataDecoder : String -> (Resource -> Decoder a) -> List Resource -> Decoder a
resourceDataDecoder type_ decoder included =
    field "data" (dataDecoder type_ decoder included)
        |> andThen (Maybe.map succeed >> Maybe.withDefault (fail "data type not found"))


dataDecoder : String -> (Resource -> Decoder a) -> List Resource -> Decoder (Maybe a)
dataDecoder type_ decoder =
    resourceInfoInternalDecoder >> andThen (filterDataType type_ decoder)


filterDataType : String -> (Resource -> Decoder a) -> Internal.ResourceInfoInternal -> Decoder (Maybe a)
filterDataType dataType decoder info =
    if dataType == info.type_ then
        field "attributes" (decoder (Internal.ResourceInfo info)) |> map Just

    else
        succeed Nothing


resourceInfoInternalDecoder : List Resource -> Decoder Internal.ResourceInfoInternal
resourceInfoInternalDecoder included =
    succeed (Internal.ResourceInfoInternal included)
        |> andMap (field "id" string |> map Just)
        |> andMap (oneOf [ field "links" linksDecoder, succeed Dict.empty ])
        |> andMap (field "type" string)
        |> andMap (oneOf [ field "relationships" resourceRelationshipsDecoder, succeed Dict.empty ])
        |> andMap (field "attributes" value)


linksDecoder : Decoder (Dict String String)
linksDecoder =
    dict string


resourceRelationshipsDecoder : Decoder (Dict String Internal.Relationship)
resourceRelationshipsDecoder =
    dict resourceRelationshipDecoder


resourceRelationshipDecoder : Decoder Internal.Relationship
resourceRelationshipDecoder =
    succeed Internal.Relationship
        |> andMap (field "data" resourceOneOrMoreRelationshipDataDecoder)
        |> andMap (oneOf [ field "links" linksDecoder, succeed Dict.empty ])


resourceOneOrMoreRelationshipDataDecoder : Decoder Internal.OneOrMoreRelationshipData
resourceOneOrMoreRelationshipDataDecoder =
    oneOf
        [ resourceRelationshipDataDecoder |> map Internal.One
        , list resourceRelationshipDataDecoder |> map Internal.Many
        , succeed Internal.NoRelationship
        ]


resourceRelationshipDataDecoder : Decoder Internal.RelationshipData
resourceRelationshipDataDecoder =
    succeed Internal.RelationshipData
        |> andMap (field "id" string)
        |> andMap (field "type" string)


findRelationship : List Resource -> Internal.OneOrMoreRelationshipData -> Maybe Resource
findRelationship included oneOrMoreRelationshipData =
    case oneOrMoreRelationshipData of
        Internal.One relationshipData ->
            List.Extra.find (isGoodRelationship relationshipData) included

        Internal.Many _ ->
            Nothing

        Internal.NoRelationship ->
            Nothing


findRelationships : List Resource -> Internal.OneOrMoreRelationshipData -> Maybe (List Resource)
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
                                    |> Maybe.map (\a -> (::) a list)
                    )
                    (Just [])
                |> Maybe.map List.reverse

        Internal.NoRelationship ->
            Nothing


isGoodRelationship : Internal.RelationshipData -> Resource -> Bool
isGoodRelationship relationshipData (Internal.ResourceInfo { id, type_ }) =
    id == Just relationshipData.id && type_ == relationshipData.type_


includedDecoder : Decoder (List Resource)
includedDecoder =
    list (resourceInfoInternalDecoder [])
        |> map (List.map Internal.ResourceInfo)


decodeRelationship : (Resource -> Decoder a) -> Resource -> Decoder a
decodeRelationship decoder (Internal.ResourceInfo info) =
    case decodeValue (decoder (Internal.ResourceInfo info)) info.attributes of
        Ok res ->
            succeed res

        Err err ->
            fail (errorToString err)


decodeRelationships : (Resource -> Decoder a) -> List Resource -> Decoder (List a)
decodeRelationships decoder =
    List.foldl
        (\(Internal.ResourceInfo info) res ->
            case res of
                Nothing ->
                    Nothing

                Just list ->
                    decodeValue (decoder (Internal.ResourceInfo info)) info.attributes
                        |> Result.toMaybe
                        |> Maybe.map (\a -> (::) a list)
        )
        (Just [])
        >> Maybe.map List.reverse
        >> Maybe.map succeed
        >> Maybe.withDefault (fail "decode relationships failed")
