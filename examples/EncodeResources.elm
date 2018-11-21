module DecodeResources exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, li, p, sup, text, textarea, ul)
import Html.Attributes exposing (style)
import Json.Encode exposing (encode, object, string)
import JsonApi exposing (ResourceInfo)
import JsonApi.Encode as Encode


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
creatorToResource creator_ =
    JsonApi.build "creators"
        |> JsonApi.withId creator_.id
        |> JsonApi.withLinks creator_.links
        |> JsonApi.withAttributes
            [ ( "firstname", string creator_.firstname )
            , ( "lastname", string creator_.lastname )
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
        Encode.resources (List.map postToResource posts)
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


