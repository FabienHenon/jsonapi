# jsonapi [![Build Status](https://travis-ci.org/FabienHenon/jsonapi.svg?branch=master)](https://travis-ci.org/FabienHenon/jsonapi)

```
elm install FabienHenon/jsonapi
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
import JsonApi.Resource exposing (Resource)
import JsonApi.Decode as Decode
import Json.Decode as JD exposing (map4, succeed, field, string, map6, Decoder)

commentDecoder : Resource -> Decoder Comment
commentDecoder resourceInfo =
    map4 Comment
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "content" string)
        (field "email" string)


creatorDecoder : Resource -> Decoder Creator
creatorDecoder resourceInfo =
    map4 Creator
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "firstname" string)
        (field "lastname" string)


postDecoder : Resource -> Decoder Post
postDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)

```

Your decoders will be passed a `Resource` containing internal information about how to decode the resources and their relationships.
From this `Resource` you can also get the resource's `id` and `links`.

Then you only need to decode your resource as usual using the decoder of your choice.

If you need to decode relationships you have 2 functions: `relationship` and `relationships`. While the former will decode a unique relationship, the later will decode a list of relationships.

These functions are given the type of the relationship to decode from the `relationships` json attribute, the `Resource` object, and the relationship decoder.

### Decoding
Finally you will want to decode your json payload and retrieve your resources:

```elm
Decode.resources "posts" postDecoder
```

By calling the function `resources` you are creating your final decoder. You have to pass it the type of your resources, your resource decoder and it will return a `Decoder` for a `JsonApi.Document` containing a `List` of your resources, **or** it will return a list of `Error` if the json api document contains an `errors` property. _You can also call `resource` that will return a `Decoder` for a `JsonApi.Document` containing only one resource if there is only one in your payload_

You can also use the functions `resourceWithMeta` and `resourcesWithMeta` that will take one more parameter which is a `Decoder` for an object corresponding to the object in the `meta` property of the json payload you want to decode.

There is also a function called `errorToFailure` that allows you to transform the result of functions like `resources` which is a `Decoder` of a `Result (List Error) (Document meta data)` directy to a `Decoder` of a `Document meta data`. With this function you are getting rid of the `Result` and in case of `errors` in the document, the `Decoder` will fail.

You can then finally decode your payload:

```elm
decode : Result String (Result (List Error) (Document NoMeta (List Post)))
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

_Note the `NoMeta` type in the `Document` type. That means you don't want any `meta` object to be decoded from the json payload. You can see an example of a `meta` object beeing decoded in the `examples/DecodeResources.elm` file_

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

### `Resource` functions

Then you will have to create a few functions to transform your data to a `Resource`:

```elm
postToResource : Post -> Resource
postToResource post =
    JsonApi.Resource.build "posts"
        |> JsonApi.Resource.withId post.id
        |> JsonApi.Resource.withLinks post.links
        |> JsonApi.Resource.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.Resource.withRelationship "creator" (JsonApi.Resource.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.Resource.withRelationship "comments" (JsonApi.Resource.relationships (List.map commentRelationship post.comments))


creatorToResource : Creator -> Resource
creatorToResource creator =
    JsonApi.Resource.build "creators"
        |> JsonApi.Resource.withId creator.id
        |> JsonApi.Resource.withLinks creator.links
        |> JsonApi.Resource.withAttributes
            [ ( "firstname", string creator.firstname )
            , ( "lastname", string creator.lastname )
            ]


commentRelationship : Comment -> ( String, Resource )
commentRelationship comment =
    ( comment.id, commentToResource comment )


commentToResource : Comment -> Resource
commentToResource comment =
    JsonApi.Resource.build "comment"
        |> JsonApi.Resource.withId comment.id
        |> JsonApi.Resource.withLinks comment.links
        |> JsonApi.Resource.withAttributes
            [ ( "content", string comment.content )
            , ( "email", string comment.email )
            ]
```

We have a function that transforms a `Post` object to a `Resource`, a `Creator` to a `Resource` and a `Comment` to a `Resource`.
Because we have many `Comment`s in a `Post` we will need a small function to create our `commentRelationship`.

### `JsonApi.Encode.Document` functions

Now that you have your `Resource`(s), we have to create a new `Document` with these resources:

```elm
myDocument : List Post -> JsonApi.Encode.Document
myDocument posts =
    JsonApi.Encode.Document.build
        |> JsonApi.Encode.Document.withResources (List.map postToResource posts)
```

Here we create a new `Document` and we add the resources in it.
There are also functions to add a `meta` object (`withMeta`) and to set the json api version (`withJsonApiVersion`).

### Encoding

Finally, once you have your document you can encode it:

```elm
encode : JsonApi.Encode.Document -> String
encode document_ =
    Encode.document document_ |> Json.Encode.encode 0
```

## Examples

To run the examples go to the `examples` directory, install dependencies and run `elm reactor`:

```
> cd examples/
> elm install
> elm reactor
```

## Tests

You can run tests with `elm-test`
