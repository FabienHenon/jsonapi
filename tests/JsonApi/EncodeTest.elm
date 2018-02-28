module JsonApi.EncodeTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import JsonApi.Encode as Encode
import JsonApi.Decode as Decode
import JsonApi exposing (ResourceInfo)
import Json.Encode exposing (string, encode)
import Json.Decode as JD exposing (decodeString, Decoder, list, field, map2)
import Dict exposing (Dict)
import JsonApi.Data.Posts exposing (..)


suite : Test
suite =
    describe "Encode"
        [ describe "resources"
            [ test "encode resources" <|
                \() ->
                    case encode 0 (Encode.resources (List.map postToResource posts)) |> decodeString (Decode.resources "posts" postDecoder) of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "encode resources with no relationship succeeds with decoder without relationship" <|
                \() ->
                    case encode 0 (Encode.resources (List.map postToResourceWithoutRelationship posts)) |> decodeString (Decode.resources "posts" postWithoutRelationshipsDecoder) of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "encode resources with no relationship fails with decoder with relationship" <|
                \() ->
                    encode 0 (Encode.resources (List.map postToResourceWithoutRelationship posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources with no link succeeds" <|
                \() ->
                    case encode 0 (Encode.resources (List.map postToResourceWithoutLinks posts)) |> decodeString (Decode.resources "posts" postDecoder) of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , []
                                        ]
                                , List.map .title >> Expect.equalLists [ "Post no link", "Post 2" ]
                                , List.map .content >> Expect.equalLists [ "Post content no link", "Post content 2" ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "encode resources with no attribute succeeds with decoder without attribute needed" <|
                \() ->
                    case encode 0 (Encode.resources (List.map postToResourceWithoutAttributes posts)) |> decodeString (Decode.resources "posts" postWithoutAttributesDecoder) of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "post-1", "post-2" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        , [ ( "self", "http://url-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "fake title", "fake title" ]
                                , List.map .content >> Expect.equalLists [ "fake content", "fake content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 1", "Comment 2" ], [ "Comment 3" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "encode resources with no attribute fails with decoder with attribute" <|
                \() ->
                    encode 0 (Encode.resources (List.map postToResourceWithoutAttributes posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources with no id fails with decoder" <|
                \() ->
                    encode 0 (Encode.resources (List.map postToResourceWithoutId posts))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            , test "encode resources makes decoding fail with an object" <|
                \() ->
                    encode 0 (Encode.resources (List.map postToResource posts))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            ]
        , describe "resource"
            [ test "encode resource" <|
                \() ->
                    case encode 0 (Encode.resource (postToResource post2)) |> decodeString (Decode.resource "posts" postDecoder) of
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "post-2"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , .title >> Expect.equal "Post 2"
                                , .content >> Expect.equal "Post content 2"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equalLists [ "Comment 3" ]
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            , test "encode resource with no relationship succeeds with decoder without relationship" <|
                \() ->
                    case encode 0 (Encode.resource (postToResourceWithoutRelationship post2)) |> decodeString (Decode.resource "posts" postWithoutRelationshipsDecoder) of
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "post-2"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , .title >> Expect.equal "Post 2"
                                , .content >> Expect.equal "Post content 2"
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            , test "encode resource with no relationship fails with decoder with relationship" <|
                \() ->
                    encode 0 (Encode.resource (postToResourceWithoutRelationship post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource with no link succeeds" <|
                \() ->
                    case encode 0 (Encode.resource (postToResourceWithoutLinks post2)) |> decodeString (Decode.resource "posts" postDecoder) of
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "post-2"
                                , .links >> Dict.toList >> Expect.equalLists []
                                , .title >> Expect.equal "Post 2"
                                , .content >> Expect.equal "Post content 2"
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            , test "encode resource with no attribute succeeds with decoder without attribute" <|
                \() ->
                    case encode 0 (Encode.resource (postToResourceWithoutAttributes post2)) |> decodeString (Decode.resource "posts" postWithoutAttributesDecoder) of
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "post-2"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://url-to-post/2" ) ]
                                , .title >> Expect.equal "fake title"
                                , .content >> Expect.equal "fake content"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equalLists [ "Comment 3" ]
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            , test "encode resource with no attribute fails with decoder with attribute" <|
                \() ->
                    encode 0 (Encode.resource (postToResourceWithoutAttributes post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource with no id fails with decoder" <|
                \() ->
                    encode 0 (Encode.resource (postToResourceWithoutId post2))
                        |> decodeString (Decode.resource "posts" postDecoder)
                        |> Expect.err
            , test "encode resource makes decoding fail with a list" <|
                \() ->
                    encode 0 (Encode.resource (postToResource post2))
                        |> decodeString (Decode.resources "posts" postDecoder)
                        |> Expect.err
            ]
        , describe "relationships"
            [ test "duplicate relationships return only one relationship in payload" <|
                \() ->
                    case encode 0 (Encode.resources (List.map postToResource [ post2, post2 ])) |> decodeString includedDecoder of
                        Ok resources ->
                            resources
                                |> List.sortBy .id
                                |> Expect.all
                                    [ List.map .id >> Expect.equalLists [ "comment-3", "creator-1" ]
                                    , List.map .type_ >> Expect.equalLists [ "comment", "creators" ]
                                    ]

                        Err error ->
                            Expect.fail error
            , test "relationships in relationships should be added in included" <|
                \() ->
                    case encode 0 (Encode.resource (groupToResource group)) |> decodeString includedDecoder of
                        Ok resources ->
                            resources
                                |> List.sortBy .id
                                |> Expect.all
                                    [ List.map .id >> Expect.equalLists [ "comment-1", "comment-2", "comment-3", "creator-1", "post-1", "post-2" ]
                                    , List.map .type_ >> Expect.equalLists [ "comment", "comment", "comment", "creators", "posts", "posts" ]
                                    ]

                        Err error ->
                            Expect.fail error
            ]
        ]


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


postToResourceWithoutId : Post -> ResourceInfo
postToResourceWithoutId post =
    JsonApi.build "posts"
        |> JsonApi.withLinks post.links
        |> JsonApi.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.withRelationship "creator" (JsonApi.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.withRelationship "comments" (JsonApi.relationships (List.map commentRelationship post.comments))


postToResourceWithoutRelationship : Post -> ResourceInfo
postToResourceWithoutRelationship post =
    JsonApi.build "posts"
        |> JsonApi.withId post.id
        |> JsonApi.withLinks post.links
        |> JsonApi.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]


postToResourceWithoutLinks : Post -> ResourceInfo
postToResourceWithoutLinks post =
    JsonApi.build "posts"
        |> JsonApi.withId post.id
        |> JsonApi.withAttributes
            [ ( "title", string post.title )
            , ( "content", string post.content )
            ]
        |> JsonApi.withRelationship "creator" (JsonApi.relationship post.creator.id (creatorToResource post.creator))
        |> JsonApi.withRelationship "comments" (JsonApi.relationships (List.map commentRelationship post.comments))


postToResourceWithoutAttributes : Post -> ResourceInfo
postToResourceWithoutAttributes post =
    JsonApi.build "posts"
        |> JsonApi.withId post.id
        |> JsonApi.withLinks post.links
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


type alias Group =
    { name : String
    , posts : List Post
    }


groupToResource : Group -> ResourceInfo
groupToResource group =
    JsonApi.build "groups"
        |> JsonApi.withAttributes
            [ ( "name", string group.name ) ]
        |> JsonApi.withRelationship "post" (JsonApi.relationships (List.map postRelationship group.posts))


postRelationship : Post -> ( String, ResourceInfo )
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
