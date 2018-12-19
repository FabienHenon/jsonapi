module JsonApi.EncodeTest exposing (Group, Included, commentRelationship, commentToResource, creatorToResource, group, groupToResource, includedDecoder, postRelationship, postToResource, postToResourceWithoutAttributes, postToResourceWithoutId, postToResourceWithoutLinks, postToResourceWithoutRelationship, suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder, decodeString, errorToString, field, list, map2)
import Json.Encode exposing (encode, string)
import JsonApi.Data.Posts exposing (..)
import JsonApi.Decode as Decode
import JsonApi.Document
import JsonApi.Encode as Encode
import JsonApi.Encode.Document as Document exposing (Document)
import JsonApi.Internal.Document
import JsonApi.Resource exposing (Resource)
import Test exposing (..)


suite : Test
suite =
    describe "Encode"
        [ describe "resources"
            [ test "encode resources" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDoc <| List.map postToResource posts)) |> decodeString (Decode.resources "posts" postDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resources with no relationship succeeds with decoder without relationship" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutRelationship posts)) |> decodeString (Decode.resources "posts" postWithoutRelationshipsDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resources with no relationship fails with decoder with relationship" <|
                \() ->
                    encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutRelationship posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources with no link succeeds" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutLinks posts)) |> decodeString (Decode.resources "posts" postDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , []
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resources with no attribute succeeds with decoder without attribute needed" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutAttributes posts)) |> decodeString (Decode.resources "posts" postWithoutAttributesDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "fake title", "fake title" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "fake content", "fake content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resources with no attribute fails with decoder with attribute" <|
                \() ->
                    encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutAttributes posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources with no id fails with decoder" <|
                \() ->
                    encode 0 (Encode.document (resourcesToDoc <| List.map postToResourceWithoutId posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources makes decoding fail with an object" <|
                \() ->
                    encode 0 (Encode.document (resourcesToDoc <| List.map postToResource posts))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            ]
        , describe "resource"
            [ test "encode resource" <|
                \() ->
                    case encode 0 (Encode.document (resourceToDoc <| postToResource post2)) |> decodeString (Decode.resource "posts" postDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "post-2"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "Post 2"
                                , JsonApi.Document.resource >> .content >> Expect.equal "Post content 2"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equalLists [ "Comment 3" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resource with no relationship succeeds with decoder without relationship" <|
                \() ->
                    case encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutRelationship post2)) |> decodeString (Decode.resource "posts" postWithoutRelationshipsDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "post-2"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "Post 2"
                                , JsonApi.Document.resource >> .content >> Expect.equal "Post content 2"
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resource with no relationship fails with decoder with relationship" <|
                \() ->
                    encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutRelationship post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource with no link succeeds" <|
                \() ->
                    case encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutLinks post2)) |> decodeString (Decode.resource "posts" postDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "post-2"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists []
                                , JsonApi.Document.resource >> .title >> Expect.equal "Post 2"
                                , JsonApi.Document.resource >> .content >> Expect.equal "Post content 2"
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resource with no attribute succeeds with decoder without attribute" <|
                \() ->
                    case encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutAttributes post2)) |> decodeString (Decode.resource "posts" postWithoutAttributesDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "post-2"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "fake title"
                                , JsonApi.Document.resource >> .content >> Expect.equal "fake content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equalLists [ "Comment 3" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode resource with no attribute fails with decoder with attribute" <|
                \() ->
                    encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutAttributes post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource with no id fails with decoder" <|
                \() ->
                    encode 0 (Encode.document (resourceToDoc <| postToResourceWithoutId post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource makes decoding fail with a list" <|
                \() ->
                    encode 0 (Encode.document (resourceToDoc <| postToResource post2))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            ]
        , describe "relationships"
            [ test "duplicate relationships return only one relationship in payload" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDoc <| List.map postToResource [ post2, post2 ])) |> decodeString includedDecoder of
                        Ok resources ->
                            resources
                                |> List.sortBy .id
                                |> Expect.all
                                    [ List.map .id >> Expect.equalLists [ "comment-3", "creator-1" ]
                                    , List.map .type_ >> Expect.equalLists [ "comment", "creators" ]
                                    ]

                        Err error ->
                            Expect.fail (errorToString error)
            , test "relationships in relationships should be added in included" <|
                \() ->
                    case encode 0 (Encode.document (resourceToDoc <| groupToResource group)) |> decodeString includedDecoder of
                        Ok resources ->
                            resources
                                |> List.sortBy .id
                                |> Expect.all
                                    [ List.map .id >> Expect.equalLists [ "comment-1", "comment-2", "comment-3", "creator-1", "post-1", "post-2" ]
                                    , List.map .type_ >> Expect.equalLists [ "comment", "comment", "comment", "creators", "posts", "posts" ]
                                    ]

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "json api"
            [ test "encode resources with json api version" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDocWithJsonApiVersion <| List.map postToResource posts)) |> decodeString (Decode.resources "posts" postDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "2.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "meta"
            [ test "encode resources with meta object" <|
                \() ->
                    case encode 0 (Encode.document (resourcesToDocWithMeta <| List.map postToResource posts)) |> decodeString (Decode.resourcesWithMeta "posts" postDecoder metaDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                , JsonApi.Document.meta >> Expect.equal { redirect = True }
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "encode only meta object" <|
                \() ->
                    case encode 0 (Encode.document docWithOnlyMeta) |> decodeString (Decode.meta metaDecoder) of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> Expect.equal JsonApi.Internal.Document.NoData
                                , JsonApi.Document.meta >> Expect.equal { redirect = True }
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        ]


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


postToResourceWithoutId : Post -> Resource
postToResourceWithoutId post =
    JsonApi.Resource.build "posts"
        |> JsonApi.Resource.withLinks post.links
        |> JsonApi.Resource.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.Resource.withRelationship "creator" (JsonApi.Resource.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.Resource.withRelationship "comments" (JsonApi.Resource.relationships (List.map commentRelationship post.comments))


postToResourceWithoutRelationship : Post -> Resource
postToResourceWithoutRelationship post =
    JsonApi.Resource.build "posts"
        |> JsonApi.Resource.withId post.id
        |> JsonApi.Resource.withLinks post.links
        |> JsonApi.Resource.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]


postToResourceWithoutLinks : Post -> Resource
postToResourceWithoutLinks post =
    JsonApi.Resource.build "posts"
        |> JsonApi.Resource.withId post.id
        |> JsonApi.Resource.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.Resource.withRelationship "creator" (JsonApi.Resource.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.Resource.withRelationship "comments" (JsonApi.Resource.relationships (List.map commentRelationship post.comments))


postToResourceWithoutAttributes : Post -> Resource
postToResourceWithoutAttributes post =
    JsonApi.Resource.build "posts"
        |> JsonApi.Resource.withId post.id
        |> JsonApi.Resource.withLinks post.links
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


type alias Group =
    { name : String
    , posts : List Post
    }


groupToResource : Group -> Resource
groupToResource group_ =
    JsonApi.Resource.build "groups"
        |> JsonApi.Resource.withAttributes
            [ ( "name", string group_.name ) ]
        |> JsonApi.Resource.withRelationship "post" (JsonApi.Resource.relationships (List.map postRelationship group.posts))


postRelationship : Post -> ( String, Resource )
postRelationship post =
    ( post.id, postToResource post )


group : Group
group =
    { name = "group 1", posts = posts }


type alias Included =
    { id : String, type_ : String }


includedDecoder : Decoder (List Included)
includedDecoder =
    field "included" <|
        list <|
            map2 Included
                (field "id" JD.string)
                (field "type" JD.string)


resourceToDoc : Resource -> Document
resourceToDoc res =
    Document.build
        |> Document.withResource res


resourcesToDoc : List Resource -> Document
resourcesToDoc res =
    Document.build
        |> Document.withResources res


resourcesToDocWithJsonApiVersion : List Resource -> Document
resourcesToDocWithJsonApiVersion res =
    Document.build
        |> Document.withResources res
        |> Document.withJsonApiVersion "2.0"


type alias Meta =
    { redirect : Bool
    }


metaDecoder : Decoder Meta
metaDecoder =
    JD.map Meta
        (field "redirect" JD.bool)


metaEncoded : Json.Encode.Value
metaEncoded =
    Json.Encode.object [ ( "redirect", Json.Encode.bool True ) ]


resourcesToDocWithMeta : List Resource -> Document
resourcesToDocWithMeta res =
    Document.build
        |> Document.withResources res
        |> Document.withMeta metaEncoded

docWithOnlyMeta : Document
docWithOnlyMeta =
    Document.build
        |> Document.withMeta metaEncoded