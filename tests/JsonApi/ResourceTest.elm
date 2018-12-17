module JsonApi.ResourceTest exposing (getInternal, suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Encode exposing (encode, object, string)
import JsonApi.Internal.ResourceInfo as Internal
import JsonApi.Resource exposing (Resource, build, id, links, relationship, relationships, withAttributes, withId, withLinks, withRelationship)
import Test exposing (..)


suite : Test
suite =
    describe "Resource"
        [ test "empty resource info has nothing except a type" <|
            \() ->
                build "test"
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships >> Dict.toList >> Expect.equalLists []
                        , getInternal .included >> Expect.equalLists []
                        ]
        , test "fromResource has resource information plus new type" <|
            \() ->
                build "test"
                    |> JsonApi.Resource.withId "test-id"
                    |> JsonApi.Resource.fromResource "new-test"
                    |> Expect.all
                        [ id >> Expect.equal "test-id"
                        , getInternal .type_ >> Expect.equal "new-test"
                        ]
        , test "resource info with id has an id" <|
            \() ->
                build "test"
                    |> withId "test-id"
                    |> Expect.all
                        [ id >> Expect.equal "test-id"
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships >> Dict.toList >> Expect.equalLists []
                        , getInternal .included >> Expect.equalLists []
                        ]
        , test "resource info with links has links" <|
            \() ->
                build "test"
                    |> withLinks (Dict.fromList [ ( "self", "http://link-1" ) ])
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists [ ( "self", "http://link-1" ) ]
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships >> Dict.toList >> Expect.equalLists []
                        , getInternal .included >> Expect.equalLists []
                        ]
        , test "resource info with attributes has attributes" <|
            \() ->
                build "test"
                    |> withAttributes [ ( "attr-1", string "1" ), ( "attr-2", string "2" ) ]
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [ ( "attr-1", string "1" ), ( "attr-2", string "2" ) ] |> encode 0)
                        , getInternal .relationships >> Dict.toList >> Expect.equalLists []
                        , getInternal .included >> Expect.equalLists []
                        ]
        , test "resource info with relationship has relationship and included" <|
            \() ->
                build "test"
                    |> withRelationship "test-rel" (relationship "rel-id" (build "rel-type"))
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships
                            >> Dict.toList
                            >> Expect.equalLists
                                [ ( "test-rel"
                                  , { data = Internal.One { id = "rel-id", type_ = "rel-type" }
                                    , links = Dict.empty
                                    }
                                  )
                                ]
                        , getInternal .included
                            >> Expect.equalLists
                                [ Internal.ResourceInfo { included = [], id = Just "rel-id", type_ = "rel-type", attributes = object [], relationships = Dict.empty, links = Dict.empty } ]
                        ]
        , test "resource info with relationships has relationships and included" <|
            \() ->
                build "test"
                    |> withRelationship "test-rel" (relationships [ ( "rel-id-1", build "rel-type-1" ), ( "rel-id-2", build "rel-type-2" ) ])
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships
                            >> Dict.toList
                            >> Expect.equalLists
                                [ ( "test-rel"
                                  , { data = Internal.Many [ { id = "rel-id-1", type_ = "rel-type-1" }, { id = "rel-id-2", type_ = "rel-type-2" } ]
                                    , links = Dict.empty
                                    }
                                  )
                                ]
                        , getInternal .included
                            >> Expect.equalLists
                                [ Internal.ResourceInfo { included = [], id = Just "rel-id-2", type_ = "rel-type-2", attributes = object [], relationships = Dict.empty, links = Dict.empty }
                                , Internal.ResourceInfo { included = [], id = Just "rel-id-1", type_ = "rel-type-1", attributes = object [], relationships = Dict.empty, links = Dict.empty }
                                ]
                        ]
        , test "resource info with multi level relationships has relationships and included" <|
            \() ->
                build "test"
                    |> withRelationship "test-rel"
                        (relationships
                            [ ( "rel-id-1", build "rel-type-1" |> withRelationship "test-rel-inner" (relationship "rel-inner-1" (build "rel-inner-type")) )
                            , ( "rel-id-2", build "rel-type-2" )
                            ]
                        )
                    |> Expect.all
                        [ id >> Expect.equal ""
                        , getInternal .type_ >> Expect.equal "test"
                        , links >> Dict.toList >> Expect.equalLists []
                        , getInternal .attributes >> encode 0 >> Expect.equal (object [] |> encode 0)
                        , getInternal .relationships
                            >> Dict.toList
                            >> Expect.equalLists
                                [ ( "test-rel"
                                  , { data = Internal.Many [ { id = "rel-id-1", type_ = "rel-type-1" }, { id = "rel-id-2", type_ = "rel-type-2" } ]
                                    , links = Dict.empty
                                    }
                                  )
                                ]
                        , getInternal .included
                            >> Expect.equalLists
                                [ Internal.ResourceInfo { included = [], id = Just "rel-id-2", type_ = "rel-type-2", attributes = object [], relationships = Dict.empty, links = Dict.empty }
                                , Internal.ResourceInfo { included = [], id = Just "rel-inner-1", type_ = "rel-inner-type", attributes = object [], relationships = Dict.empty, links = Dict.empty }
                                , Internal.ResourceInfo
                                    { included = [ Internal.ResourceInfo { included = [], id = Just "rel-inner-1", type_ = "rel-inner-type", attributes = object [], relationships = Dict.empty, links = Dict.empty } ]
                                    , id = Just "rel-id-1"
                                    , type_ = "rel-type-1"
                                    , attributes = object []
                                    , relationships =
                                        Dict.fromList
                                            [ ( "test-rel-inner"
                                              , { data = Internal.One { id = "rel-inner-1", type_ = "rel-inner-type" }
                                                , links = Dict.empty
                                                }
                                              )
                                            ]
                                    , links = Dict.empty
                                    }
                                ]
                        ]
        ]


getInternal : (Internal.ResourceInfoInternal -> b) -> Resource -> b
getInternal mapper (Internal.ResourceInfo resourceInfo) =
    mapper resourceInfo
