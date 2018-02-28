# jsonapi [![Build Status](https://travis-ci.org/FabienHenon/jsonapi.svg?branch=master)](https://travis-ci.org/FabienHenon/jsonapi)

```
elm package install FabienHenon/jsonapi
```

`JsonApi` allows you to decode and encode json content conforming to the [Json Api spec](http://jsonapi.org/).
We can decode and encode resources and their relationships.

## Decoding resources

### Types
First, you need to declare your types:

```elm
type alias Post =
    { id : String
    , links : Dict String String
    , title : String
    , content : String
    , creator : Creator
    , comments : List Comment
    }


type alias Creator =
    { id : String
    , links : Dict String String
    , firstname : String
    , lastname : String
    }


type alias Comment =
    { id : String
    , links : Dict String String
    , content : String
    , email : String
    }
```

With these records we will retrieve resources of type `Post`. These posts contain one `Creator` and one or many `Comment`s.

### Decoders
Then you have to define your resource decoder as well as the decoders for the relationships you need:

```elm
import JsonApi exposing (ResourceInfo)
import JsonApi.Decode as Decode
import Json.Decode as JD exposing (map4, succeed, field, string, map6, Decoder)

commentDecoder : ResourceInfo -> Decoder Comment
commentDecoder resourceInfo =
    map4 Comment
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "content" string)
        (field "email" string)


creatorDecoder : ResourceInfo -> Decoder Creator
creatorDecoder resourceInfo =
    map4 Creator
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "firstname" string)
        (field "lastname" string)


postDecoder : ResourceInfo -> Decoder Post
postDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)

```

Your decoders will be passed a `ResourceInfo` containing internal information about how to decode the resources and their relationships.
From this `ResourceInfo` you can also get the resource's `id` and `links`.

Then you only need to decode your resource as usual using the decoder of your choice.

If you need to decode relationships you have 2 functions: `relationship` and `relationships`. While the former will decode a unique relationship, the later will decode a list of relationships.

These functions are given the type of the relationship to decode from the `relationships` json attribute, the `ResourceInfo` object, and the relationship decoder.

### Decoding
Finally you will want to decode your json payload and retrieve your resources:

```elm
Decode.resources "posts" postDecoder
```

By calling the function `resources` you are creating your final decoder. You have to pass it the type of your resources, your resource decoder and it will return a `Decoder` for a `List` of your resource. _You can also call `resource` that will return a `Decoder a` instead of `Decoder (List a)`. Useful when you will have only one resource in your payload_

You can then finally decode your payload:

```elm
decode : Result String (List Post)
decode =
        Json.Decode.decodeString (Decode.resources "posts" postDecoder) payload


payload : String
payload =
    """
    {
        "data": [
            {
                "type": "posts",
                "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
                "links": {
                    "self": "http://link-to-post/1"
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
    """
```

## Encoding resources

### Types

You first need to declare your types:

```elm
type alias Post =
    { id : String
    , links : Dict String String
    , title : String
    , content : String
    , creator : Creator
    , comments : List Comment
    }


type alias Creator =
    { id : String
    , links : Dict String String
    , firstname : String
    , lastname : String
    }


type alias Comment =
    { id : String
    , links : Dict String String
    , content : String
    , email : String
    }
```

We will encode a list of `Post`s. Each `Post` containing a list of `Comment`s and a `Creator`.

### `ResourceInfo` functions

Then you will have to create a few functions to transform your data to `ResourceInfo`:

```elm
postToResource : Post -> ResourceInfo
postToResource post =
    JsonApi.build "posts"
        |> JsonApi.withId post.id
        |> JsonApi.withLinks post.links
        |> JsonApi.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.withRelationship "creator" (JsonApi.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.withRelationship "comments" (JsonApi.relationships (List.map commentRelationship post.comments))


creatorToResource : Creator -> ResourceInfo
creatorToResource creator =
    JsonApi.build "creators"
        |> JsonApi.withId creator.id
        |> JsonApi.withLinks creator.links
        |> JsonApi.withAttributes
            [ ( "firstname", string creator.firstname )
            , ( "lastname", string creator.lastname )
            ]


commentRelationship : Comment -> ( String, ResourceInfo )
commentRelationship comment =
    ( comment.id, commentToResource comment )


commentToResource : Comment -> ResourceInfo
commentToResource comment =
    JsonApi.build "comment"
        |> JsonApi.withId comment.id
        |> JsonApi.withLinks comment.links
        |> JsonApi.withAttributes
            [ ( "content", string comment.content )
            , ( "email", string comment.email )
            ]
```

We have a function that transforms a `Post` object to a `ResourceInfo`, a `Creator` to a `ResourceInfo` and a `Comment` to a `ResourceInfo`.
Because we have many `Comment`s in a `Post` we will need a small function to creator our `commentRelationship`.

### Encoding

Finally, once you have your data you can encode it:

```elm
encode : List Post -> String
encode posts =
    Encode.resources (List.map postToResource posts) |> Json.Encode.encode 0
```

## Examples

To run the examples go to the `examples` directory, install dependencies and run `elm-reactor`:

```
> cd examples/
> elm package install
> elm-reactor
```

## Tests

You can install elm packages in tests folder and run tests with `elm-test`
