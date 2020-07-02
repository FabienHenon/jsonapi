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
import JsonApi.Resource
import Test exposing (..)


suite : Test
suite =
    describe "Decode"
        [ describe "resources"
            [ test "decode success" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.validPayload of
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
            , test "decode success with null link" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.validPayloadWithNullLink of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad", "13608770-76dd-47e5-a1c4-4d0d9c2483ae" ]
                                , JsonApi.Document.resource
                                    >> List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "other", "http://link-to-post/other" ), ( "self", "http://link-to-post/1" ) ]
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
                    case decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.validPayloadWithoutLinks of
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
            , test "decode success with root links" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.validPayloadWithRootLinks of
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
                                , JsonApi.Document.links
                                    >> Dict.toList
                                    >> Expect.equalLists
                                        [ ( "other", "http://root/2" )
                                        , ( "self", "http://root/1" )
                                        ]
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode success with null relationship" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoderWithoutCreator |> Decode.errorToFailure) Resources.validPayloadWithNullRelationship of
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
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutData
                        |> Expect.err
            , test "decode failed with missing attributes field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutAttributes
                        |> Expect.err
            , test "decode failed with missing id field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutId
                        |> Expect.err
            , test "decode failed with missing type field" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutType
                        |> Expect.err
            , test "decode resources failed" <|
                \() ->
                    decodeString (Decode.resources "posts" badPostDecoder |> Decode.errorToFailure) Resources.validPayload
                        |> Expect.err
            , test "decode failed with bad creatorDecoder" <|
                \() ->
                    decodeString (Decode.resources "posts" postBadCreatorDecoder |> Decode.errorToFailure) Resources.validPayload
                        |> Expect.err
            , test "decode failed with relationship not in relationships" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutRelationshipInRelationships
                        |> Expect.err
            , test "decode failed with relationship id not found" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutRelationshipIdNotFound
                        |> Expect.err
            , test "decode failed with relationship type not found" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutRelationshipTypeNotFound
                        |> Expect.err
            , test "decode failed with relationship not in included" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutRelationshipInIncluded
                        |> Expect.err
            , test "decode succeed with missing relationships" <|
                \() ->
                    case decodeString (Decode.resources "posts" postWithoutRelationshipsDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutRelationships of
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
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutCreatorAttributes
                        |> Expect.err
            , test "decode failed with missing id field for Creator" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutCreatorId
                        |> Expect.err
            , test "decode failed with missing type field for Creator" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutCreatorType
                        |> Expect.err
            , test "decode succeed with missing relationships for Creator" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadWithoutCreatorRelationships of
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
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadCreatorIsList
                        |> Expect.err
            , test "decode failed with one owner instead of list" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.invalidPayloadCommentsIsOneElement
                        |> Expect.err
            , test "decode failed with one data object instead of list" <|
                \() ->
                    decodeString (Decode.resources "posts" postDecoder |> Decode.errorToFailure) Resources.dataIsObject
                        |> Expect.err
            , test "decode success with relationships desc" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoderWithRelationshipsDesc |> Decode.errorToFailure) Resources.validPayload of
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
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                , JsonApi.Document.resource
                                    >> List.map
                                        (.relationships
                                            >> Dict.toList
                                            >> List.map (Tuple.mapSecond (.links >> Dict.toList))
                                        )
                                    >> Expect.equalLists
                                        [ [ ( "comments", [] )
                                          , ( "creator", [ ( "related", "http://link-to-creator/1" ) ] )
                                          ]
                                        , [ ( "comments", [] )
                                          , ( "creator", [ ( "related", "http://lnk-to-creator/1" ) ] )
                                          ]
                                        ]
                                , JsonApi.Document.resource
                                    >> List.map
                                        (.relationships
                                            >> Dict.toList
                                            >> List.map (Tuple.mapSecond (.data >> relationshipToList >> List.map .id))
                                        )
                                    >> Expect.equalLists
                                        [ [ ( "comments", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ab", "cb0759b0-03ab-4291-b067-84a9017fea6f" ] )
                                          , ( "creator", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ad" ] )
                                          ]
                                        , [ ( "comments", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ac" ] )
                                          , ( "creator", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ad" ] )
                                          ]
                                        ]
                                , JsonApi.Document.resource
                                    >> List.map
                                        (.relationships
                                            >> Dict.toList
                                            >> List.map (Tuple.mapSecond (.data >> relationshipToList >> List.map .type_))
                                        )
                                    >> Expect.equalLists
                                        [ [ ( "comments", [ "comment", "comment" ] )
                                          , ( "creator", [ "creators" ] )
                                          ]
                                        , [ ( "comments", [ "comment" ] )
                                          , ( "creator", [ "creators" ] )
                                          ]
                                        ]
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "resource"
            [ test "decode success" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.validPayload of
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
            , test "decode success with null link" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.validPayloadWithNullLink of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "other", "http://link-to-post/other" ), ( "self", "http://link-to-post/1" ) ]
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
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.validPayloadWithoutLinks of
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
            , test "decode success with root links" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.validPayloadWithRootLinks of
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
                                , JsonApi.Document.links
                                    >> Dict.toList
                                    >> Expect.equalLists
                                        [ ( "other", "http://root/2" )
                                        , ( "self", "http://root/1" )
                                        ]
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode faild with missing data field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutData
                        |> Expect.err
            , test "decode failed with missing attributes field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutAttributes
                        |> Expect.err
            , test "decode failed with missing id field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutId
                        |> Expect.err
            , test "decode failed with missing type field" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutType
                        |> Expect.err
            , test "decode resource failed" <|
                \() ->
                    decodeString (Decode.resource "posts" badPostDecoder |> Decode.errorToFailure) Resource.validPayload
                        |> Expect.err
            , test "decode failed with bad creatorDecoder" <|
                \() ->
                    decodeString (Decode.resource "posts" postBadCreatorDecoder |> Decode.errorToFailure) Resource.validPayload
                        |> Expect.err
            , test "decode failed with relationship not in relationships" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutRelationshipInRelationships
                        |> Expect.err
            , test "decode failed with relationship id not found" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutRelationshipIdNotFound
                        |> Expect.err
            , test "decode failed with relationship type not found" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutRelationshipTypeNotFound
                        |> Expect.err
            , test "decode failed with relationship not in included" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutRelationshipInIncluded
                        |> Expect.err
            , test "decode succeed with missing relationships" <|
                \() ->
                    case decodeString (Decode.resource "posts" postWithoutRelationshipsDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutRelationships of
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
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutCreatorAttributes
                        |> Expect.err
            , test "decode failed with missing id field for Creator" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutCreatorId
                        |> Expect.err
            , test "decode failed with missing type field for Creator" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutCreatorType
                        |> Expect.err
            , test "decode succeed with missing relationships for Creator" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadWithoutCreatorRelationships of
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
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadCreatorIsList
                        |> Expect.err
            , test "decode failed with one owner instead of list" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.invalidPayloadCommentsIsOneElement
                        |> Expect.err
            , test "decode failed with data as list instead of object" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) Resource.dataIsList
                        |> Expect.err
            , test "included is not needed" <|
                \() ->
                    case decodeString (Decode.resource "comments" commentDecoder |> Decode.errorToFailure) Resource.commentWithoutIncluded of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .content >> Expect.equal "Comment content"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode success with relationships desc" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoderWithRelationshipsDesc |> Decode.errorToFailure) Resource.validPayload of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , JsonApi.Document.resource >> .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , JsonApi.Document.resource >> .title >> Expect.equal "First post"
                                , JsonApi.Document.resource >> .content >> Expect.equal "First post content"
                                , JsonApi.Document.meta >> Expect.equal JsonApi.Internal.Document.NoMeta
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                , JsonApi.Document.resource
                                    >> .relationships
                                    >> Dict.toList
                                    >> List.map (Tuple.mapSecond (.links >> Dict.toList))
                                    >> Expect.equalLists
                                        [ ( "comments", [] )
                                        , ( "creator", [ ( "related", "http://link-to-creator/1" ) ] )
                                        ]
                                , JsonApi.Document.resource
                                    >> .relationships
                                    >> Dict.toList
                                    >> List.map (Tuple.mapSecond (.data >> relationshipToList >> List.map .id))
                                    >> Expect.equalLists
                                        [ ( "comments", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ab", "cb0759b0-03ab-4291-b067-84a9017fea6f" ] )
                                        , ( "creator", [ "22208770-76dd-47e5-a1c4-4d0d9c2483ad" ] )
                                        ]
                                , JsonApi.Document.resource
                                    >> .relationships
                                    >> Dict.toList
                                    >> List.map (Tuple.mapSecond (.data >> relationshipToList >> List.map .type_))
                                    >> Expect.equalLists
                                        [ ( "comments", [ "comment", "comment" ] )
                                        , ( "creator", [ "creators" ] )
                                        ]
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            ]
        , describe "json api version"
            [ test "decode succeed with correct json api version" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) DocRes.validPayloadJsonApiVersion of
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
                    case decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) DocRes.validPayloadWithBadJsonApiVersion of
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
                    case decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder |> Decode.errorToFailure) DocRes.validPayloadMeta of
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
            , test "decode meta succeed even with no data object" <|
                \() ->
                    case decodeString (Decode.meta metaDecoder |> Decode.errorToFailure) DocRes.validPayloadOnlyMeta of
                        Ok document ->
                            Expect.all
                                [ JsonApi.Document.resource >> Expect.equal JsonApi.Internal.Document.NoData
                                , JsonApi.Document.meta >> Expect.equal { redirect = True }
                                , JsonApi.Document.jsonApiVersion >> Expect.equal "1.0"
                                ]
                                document

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with no meta object" <|
                \() ->
                    decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder |> Decode.errorToFailure) DocRes.validPayloadNoMeta
                        |> Expect.err
            , test "decode failed with invalid meta object" <|
                \() ->
                    decodeString (Decode.resourceWithMeta "posts" postDecoder metaDecoder |> Decode.errorToFailure) DocRes.validPayloadBadMeta
                        |> Expect.err
            ]
        , describe "errors"
            [ test "decode failed with bad formatted errors (not list)" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder) DocRes.invalidErrorPayload
                        |> Expect.err
            , test "decode succeed with errors have priority over data" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) DocRes.validErrorPayloadWithData of
                        Ok result ->
                            case result of
                                Err errors ->
                                    Expect.all
                                        [ List.map .title >> Expect.equalLists [ Just "My error", Just "My error 2" ]
                                        , List.map .id >> Expect.equalLists [ Nothing, Just "error-id" ]
                                        ]
                                        errors

                                Ok _ ->
                                    Expect.fail "Has data but should be errors"

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode succeed with errors" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) DocRes.validErrorPayload of
                        Ok result ->
                            case result of
                                Err errors ->
                                    Expect.all
                                        [ List.map .title >> Expect.equalLists [ Just "My error", Just "My error 2" ]
                                        , List.map .id >> Expect.equalLists [ Nothing, Just "error-id" ]
                                        ]
                                        errors

                                Ok _ ->
                                    Expect.fail "Has data but should be errors"

                        Err error ->
                            Expect.fail (errorToString error)
            , test "decode failed with errorToFailure" <|
                \() ->
                    decodeString (Decode.resource "posts" postDecoder |> Decode.errorToFailure) DocRes.validErrorPayload
                        |> Expect.err
            ]
        ]


relationshipToList : JsonApi.Resource.OneOrMoreRelationshipData -> List JsonApi.Resource.RelationshipData
relationshipToList oneOrMore =
    case oneOrMore of
        JsonApi.Resource.NoRelationship ->
            []

        JsonApi.Resource.One r ->
            [ r ]

        JsonApi.Resource.Many r ->
            r
