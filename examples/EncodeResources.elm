module DecodeResources exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, li, p, sup, text, textarea, ul)
import Html.Attributes exposing (style)
import Json.Encode exposing (encode, object, string)
import JsonApi.Encode as Encode
import JsonApi.Encode.Document as Document exposing (Document)
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


posts : List Post
posts =
    [ postNoLink, post2 ]


postNoLink : Post
postNoLink =
    { id = "post-1"
    , links = Dict.empty
    , title = "Post no link"
    , content = "Post content no link"
    , creator = creator
    , comments = [ comment1, comment2 ]
    }


post2 : Post
post2 =
    { id = "post-2"
    , links = Dict.fromList [ ( "self", "http://url-to-post/2" ) ]
    , title = "Post 2"
    , content = "Post content 2"
    , creator = creator
    , comments = [ comment3 ]
    }


creator : Creator
creator =
    { id = "creator-1"
    , links = Dict.fromList [ ( "self", "http://url-to-creator/1" ) ]
    , firstname = "John"
    , lastname = "Doe"
    }


comment1 : Comment
comment1 =
    { id = "comment-1"
    , links = Dict.fromList [ ( "self", "http://url-to-comment/1" ) ]
    , content = "Comment 1"
    , email = "email@email.com"
    }


comment2 : Comment
comment2 =
    { id = "comment-2"
    , links = Dict.fromList [ ( "self", "http://url-to-comment/2" ) ]
    , content = "Comment 2"
    , email = "email@email.com"
    }


comment3 : Comment
comment3 =
    { id = "comment-3"
    , links = Dict.fromList [ ( "self", "http://url-to-comment/3" ) ]
    , content = "Comment 3"
    , email = "email@email.com"
    }


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
creatorToResource creator_ =
    JsonApi.Resource.build "creators"
        |> JsonApi.Resource.withId creator_.id
        |> JsonApi.Resource.withLinks creator_.links
        |> JsonApi.Resource.withAttributes
            [ ( "firstname", string creator_.firstname )
            , ( "lastname", string creator_.lastname )
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


encodeMeta : Json.Encode.Value
encodeMeta =
    object
        [ ( "redirect", Json.Encode.bool True ) ]


myDocument : Document
myDocument =
    Document.build
        |> Document.withMeta encodeMeta
        |> Document.withJsonApiVersion "2.0"
        |> Document.withResources (List.map postToResource posts)


type Msg
    = NoOp


type alias Model =
    { posts : String
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
    { posts =
        Encode.document myDocument
            |> encode 4
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
        [ textarea
            [ style "position" "fixed"
            , style "width" "100%"
            , style "height" "100%"
            , style "font-size" "18px"
            , style "font-familly" "Courrier New"
            ]
            [ text model.posts ]
        ]
