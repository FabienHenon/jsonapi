module JsonApi.DecodeTest exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode exposing (Decoder, decodeString, errorToString, field, map4, map6, string, succeed)
import JsonApi.Data.DocumentPayloads as DocRes
import JsonApi.Data.Posts exposing (..)
import JsonApi.Data.ResourcePayloads as Resource
import JsonApi.Data.ResourcesPayloads as Resources
import JsonApi.Decode as Decode
import JsonApi.Document
import JsonApi.Internal.Document
import Test exposing (..)


suite : Test
suite =
    describe "Decode"
        [ describe "resources"
            [ test "decode success" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder) Resources.validPayload of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad", "13608770-76dd-47e5-a1c4-4d0d9c2483ae" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/1" ) ]
                                        , [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "First post", "Second post" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "First post content", "Second post content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ], [ "Comment 1 content" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode success with missing links" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder) Resources.validPayloadWithoutLinks of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "First post" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "First post content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode success with null relationship" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoderWithoutCreator) Resources.validPayloadWithNullRelationship of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/1" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "First post" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "First post content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "Fake" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Fake" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode faild with missing data field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutData
                        |> Expect.err
            , test "decode failed with missing attributes field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutAttributes
                        |> Expect.err
            , test "decode failed with missing id field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutId
                        |> Expect.err
            , test "decode failed with missing type field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutType
                        |> Expect.err
            , test "decode resources failed" <|
                \() ->
                    decodeString (Decode.resources "posts" badPostDecoder) Resources.validPayload
                        |> Expect.err
            , test "decode failed with bad creatorDecoder" <|
                \() ->
                    decodeString (Decode.resources "posts" postBadCreatorDecoder) Resources.validPayload
                        |> Expect.err
            , test "decode failed with relationship not in relationships" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutRelationshipInRelationships
                        |> Expect.err
            , test "decode failed with relationship id not found" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutRelationshipIdNotFound
                        |> Expect.err
            , test "decode failed with relationship type not found" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutRelationshipTypeNotFound
                        |> Expect.err
            , test "decode failed with relationship not in included" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutRelationshipInIncluded
                        |> Expect.err
            , test "decode succeed with missing relationships" <|
                \() ->
                    case decodeString (Decode.resources "posts" postWithoutRelationshipsDecoder) Resources.invalidPayloadWithoutRelationships of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "First post" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "First post content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with missing attributes field for Creator" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutCreatorAttributes
                        |> Expect.err
            , test "decode failed with missing id field for Creator" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutCreatorId
                        |> Expect.err
            , test "decode failed with missing type field for Creator" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutCreatorType
                        |> Expect.err
            , test "decode succeed with missing relationships for Creator" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadWithoutCreatorRelationships of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , JsonApi.Document.resource >> List.map .title >> Expect.equalLists [ "First post" ]
                                , JsonApi.Document.resource >> List.map .content >> Expect.equalLists [ "First post content" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , JsonApi.Document.resource >> List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , JsonApi.Document.resource >> List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with list of creators instead of one element" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadCreatorIsList
                        |> Expect.err
            , test "decode failed with one owner instead of list" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.invalidPayloadCommentsIsOneElement
                        |> Expect.err
            , test "decode failed with one data object instead of list" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder) Resources.dataIsObject
                        |> Expect.err
            ]
        , describe "resource"
            [ test "decode success" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) Resource.validPayload of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equalLists [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode success with missing links" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) Resource.validPayloadWithoutLinks of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists []
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equalLists [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode faild with missing data field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutData
                        |> Expect.err
            , test "decode failed with missing attributes field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutAttributes
                        |> Expect.err
            , test "decode failed with missing id field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutId
                        |> Expect.err
            , test "decode failed with missing type field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutType
                        |> Expect.err
            , test "decode resource failed" <|
                \() ->
                    decodeString (Decode.resource "posts" badPostDecoder) Resource.validPayload
                        |> Expect.err
            , test "decode failed with bad creatorDecoder" <|
                \() ->
                    decodeString (Decode.resource "posts" postBadCreatorDecoder) Resource.validPayload
                        |> Expect.err
            , test "decode failed with relationship not in relationships" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutRelationshipInRelationships
                        |> Expect.err
            , test "decode failed with relationship id not found" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutRelationshipIdNotFound
                        |> Expect.err
            , test "decode failed with relationship type not found" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutRelationshipTypeNotFound
                        |> Expect.err
            , test "decode failed with relationship not in included" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutRelationshipInIncluded
                        |> Expect.err
            , test "decode succeed with missing relationships" <|
                \() ->
                    case decodeString (Decode.resource "posts" postWithoutRelationshipsDecoder) Resource.invalidPayloadWithoutRelationships of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/2" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equal []
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with missing attributes field for Creator" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutCreatorAttributes
                        |> Expect.err
            , test "decode failed with missing id field for Creator" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutCreatorId
                        |> Expect.err
            , test "decode failed with missing type field for Creator" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutCreatorType
                        |> Expect.err
            , test "decode succeed with missing relationships for Creator" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadWithoutCreatorRelationships of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/2" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equal [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with list of creators instead of one element" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadCreatorIsList
                        |> Expect.err
            , test "decode failed with one owner instead of list" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.invalidPayloadCommentsIsOneElement
                        |> Expect.err
            , test "decode failed with data as list instead of object" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) Resource.dataIsList
                        |> Expect.err
            , test "included is not needed" <|
                \() ->
                    case decodeString (Decode.resource "comments" commentDecoder) Resource.commentWithoutIncluded of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .content >> Expect.equal "Comment content"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "json api version"
            [ test "decode succeed with correct json api version" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) DocRes.validPayloadJsonApiVersion of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equal [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "2.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode succeed with invalid json api version" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) DocRes.validPayloadWithBadJsonApiVersion of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equal [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "meta"
            [ test "decode resource succeed with correct meta decoder" <|
                \() ->
                    case decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder) DocRes.validPayloadMeta of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.resource >> .creator >> .firstname >> Expect.equal "John"
                                , JsonApi.Document.resource >> .creator >> .lastname >> Expect.equal "Doe"
                                , JsonApi.Document.resource >> .comments >> List.map .content >> Expect.equal [ "Comment 2 content", "Comment 3 content" ]
                                , JsonApi.Document.meta >> Expect.equal { redirect = True }
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with no meta object" <|
                \() ->
                    decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder) DocRes.validPayloadNoMeta
                        |> Expect.err
            , test "decode failed with invalid meta object" <|
                \() ->
                    decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder) DocRes.validPayloadBadMeta
                        |> Expect.err
            ]
        ]
