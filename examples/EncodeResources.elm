module DecodeResources exposing (main)

import Html exposing (Html, div, ul, li, text, h1, p, sup, textarea)
import Html.Attributes exposing (style)
import Json.Encode exposing (encode, string, object)
import JsonApi exposing (ResourceInfo)
import JsonApi.Encode as Encode
import Dict exposing (Dict)


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


type Msg
    = NoOp


type alias Model =
    { posts : String
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { posts =
        Encode.resources (List.map postToResource posts)
            |> encode 4
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ textarea
            [ style
                [ ( "position", "fixed" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "font-size", "18px" )
                , ( "font-familly", "Courrier New" )
                ]
            ]
            [ text model.posts ]
        ]


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
