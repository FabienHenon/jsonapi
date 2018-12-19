module DecodeResources exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, li, p, sup, text, ul)
import Json.Decode as JD exposing (Decoder, bool, field, map, map4, map6, string, succeed)
import JsonApi.Decode as Decode
import JsonApi.Document exposing (Document)
import JsonApi.Resource exposing (Resource)


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


type alias Meta =
    { redirect : Bool
    }


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


metaDecoder : Decoder Meta
metaDecoder =
    map Meta
        (field "redirect" bool)


type Msg
    = NoOp


type alias Model =
    { document : Maybe (Document Meta (List Post))
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


initModel : Model
initModel =
    { document =
        Decode.resourcesWithMeta "posts" postDecoder metaDecoder
            |> Decode.errorToFailure
            |> (\a -> JD.decodeString a payload)
            |> Result.toMaybe
    }


init : Model
init =
    initModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div
        []
        (case model.document of
            Nothing ->
                [ text "No post available" ]

            Just document ->
                [ div []
                    [ if (JsonApi.Document.meta document).redirect then
                        text "Redirect"

                      else
                        text "No redirect"
                    ]
                , ul []
                    (document
                        |> JsonApi.Document.resource
                        |> List.map viewPost
                    )
                ]
        )


viewPost : Post -> Html Msg
viewPost post =
    li []
        [ h1 [] [ text post.title ]
        , div []
            [ sup [] [ text ("Author: " ++ post.creator.firstname ++ " " ++ post.creator.lastname) ]
            , p [] [ text post.content ]
            , ul []
                (List.map viewComment post.comments)
            ]
        ]


viewComment : Comment -> Html Msg
viewComment comment =
    li []
        [ sup [] [ text ("Email: " ++ comment.email) ]
        , p [] [ text comment.content ]
        ]


payload : String
payload =
    """
    {
        "meta": {
            "redirect": true
        },
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
