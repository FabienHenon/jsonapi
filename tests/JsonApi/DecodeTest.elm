module JsonApi.DecodeTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import JsonApi.Decode as Decode
import JsonApi.Data.ResourcesPayloads as Resources
import JsonApi.Data.ResourcePayloads as Resource
import Json.Decode exposing (Decoder, field, string, succeed, decodeString, map4, map6)
import Dict exposing (Dict)
import JsonApi.Data.Posts exposing (..)


suite : Test
suite =
    describe "Decode"
        [ describe "resources"
            [ test "decode success" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder) Resources.validPayload of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad", "13608770-76dd-47e5-a1c4-4d0d9c2483ae" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/1" ) ]
                                        , [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "First post", "Second post" ]
                                , List.map .content >> Expect.equalLists [ "First post content", "Second post content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John", "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe", "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ], [ "Comment 1 content" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "decode success with missing links" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoder) Resources.validPayloadWithoutLinks of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ []
                                        ]
                                , List.map .title >> Expect.equalLists [ "First post" ]
                                , List.map .content >> Expect.equalLists [ "First post content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
            , test "decode success with null relationship" <|
                \() ->
                    case decodeString (Decode.resources "posts" postDecoderWithoutCreator) Resources.validPayloadWithNullRelationship of
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/1" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "First post" ]
                                , List.map .content >> Expect.equalLists [ "First post content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "Fake" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Fake" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
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
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "First post" ]
                                , List.map .content >> Expect.equalLists [ "First post content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
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
                        Ok resources ->
                            Expect.all
                                [ List.map .id >> Expect.equalLists [ "13608770-76dd-47e5-a1c4-4d0d9c2483ad" ]
                                , List.map (.links >> Dict.toList)
                                    >> Expect.equalLists
                                        [ [ ( "self", "http://link-to-post/2" ) ]
                                        ]
                                , List.map .title >> Expect.equalLists [ "First post" ]
                                , List.map .content >> Expect.equalLists [ "First post content" ]
                                , List.map (.creator >> .firstname) >> Expect.equalLists [ "John" ]
                                , List.map (.creator >> .lastname) >> Expect.equalLists [ "Doe" ]
                                , List.map (.comments >> List.map .content) >> Expect.equalLists [ [ "Comment 2 content", "Comment 3 content" ] ]
                                ]
                                resources

                        Err error ->
                            Expect.fail error
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
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/1" ) ]
                                , .title >> Expect.equal "First post"
                                , .content >> Expect.equal "First post content"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equalLists [ "Comment 2 content", "Comment 3 content" ]
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            , test "decode success with missing links" <|
                \() ->
                    case decodeString (Decode.resource "posts" postDecoder) Resource.validPayloadWithoutLinks of
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , .links >> Dict.toList >> Expect.equalLists []
                                , .title >> Expect.equal "First post"
                                , .content >> Expect.equal "First post content"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equalLists [ "Comment 2 content", "Comment 3 content" ]
                                ]
                                resource

                        Err error ->
                            Expect.fail error
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
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/2" ) ]
                                , .title >> Expect.equal "First post"
                                , .content >> Expect.equal "First post content"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equal []
                                ]
                                resource

                        Err error ->
                            Expect.fail error
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
                        Ok resource ->
                            Expect.all
                                [ .id >> Expect.equal "13608770-76dd-47e5-a1c4-4d0d9c2483ad"
                                , .links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-to-post/2" ) ]
                                , .title >> Expect.equal "First post"
                                , .content >> Expect.equal "First post content"
                                , .creator >> .firstname >> Expect.equal "John"
                                , .creator >> .lastname >> Expect.equal "Doe"
                                , .comments >> List.map .content >> Expect.equal [ "Comment 2 content", "Comment 3 content" ]
                                ]
                                resource

                        Err error ->
                            Expect.fail error
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
                        Ok resource ->
                            Expect.all
                                [ .content >> Expect.equal "Comment content"
                                ]
                                resource

                        Err error ->
                            Expect.fail error
            ]
        ]
