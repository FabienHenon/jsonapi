module JsonApi.Encode exposing (resource, resources)

{-| Provides functions to encode resources to a `Json.Encode.Value`. You can then finally
encode it to a json api string with `Json.Encode.encode`.


# Encoders

@docs resource, resources

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value, list, null, object, string)
import JsonApi exposing (OneOrManyRelationships, ResourceInfo)
import JsonApi.Internal.ResourceInfo as Internal


{-| Encodes a list of resources.

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

    resources posts

-}
resources : List ResourceInfo -> Value
resources resources_ =
    encodeBasePayload (getAllIncluded resources_) (encodeResources resources_)


{-| Encodes a resource.

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

    post : Post
    post =
        { id = "post-1"
        , links = Dict.fromList [ ( "self", "http://url-to-post/1" ) ]
        , title = "Post 1"
        , content = "Post content 1"
        , creator = creator
        , comments = [ comment1 ]
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

    resource post

-}
resource : ResourceInfo -> Value
resource (Internal.ResourceInfo resource_) =
    encodeBasePayload resource_.included (encodeResource (Internal.ResourceInfo resource_))



-- LOGIC


encodeBasePayload : List ResourceInfo -> Value -> Value
encodeBasePayload included data =
    object
        [ ( "data", data )
        , ( "included", list encodeResource included )
        ]


encodeResources : List ResourceInfo -> Value
encodeResources resources_ =
    list encodeResource resources_


encodeResource : ResourceInfo -> Value
encodeResource (Internal.ResourceInfo { id, type_, attributes, relationships, links }) =
    object
        (encodeOptionalId id
            ++ encodeOptionalLinks links
            ++ [ ( "type", string type_ )
               , ( "attributes", attributes )
               , ( "relationships", encodeRelationships relationships )
               ]
        )


encodeOptionalId : Maybe String -> List ( String, Value )
encodeOptionalId =
    Maybe.map (\id -> [ ( "id", string id ) ]) >> Maybe.withDefault []


encodeOptionalLinks : Dict String String -> List ( String, Value )
encodeOptionalLinks links =
    case Dict.toList links of
        [] ->
            []

        l ->
            [ ( "links", object (List.map (\( k, v ) -> ( k, string v )) l) ) ]


encodeRelationships : Dict String Internal.Relationship -> Value
encodeRelationships relationships =
    object
        (relationships |> Dict.toList |> List.map encodeRelationship)


encodeRelationship : ( String, Internal.Relationship ) -> ( String, Value )
encodeRelationship ( type_, relationship ) =
    ( type_, encodeRelationshipData relationship )


encodeRelationshipData : Internal.Relationship -> Value
encodeRelationshipData relationship =
    object
        (encodeOptionalLinks relationship.links
            ++ [ ( "data", encodeRelationshipOneOrMoreData relationship.data )
               ]
        )


encodeRelationshipOneOrMoreData : Internal.OneOrMoreRelationshipData -> Value
encodeRelationshipOneOrMoreData data =
    case data of
        Internal.One d ->
            encodeOneRelationshipData d

        Internal.Many d ->
            list encodeOneRelationshipData d

        Internal.NoRelationship ->
            null


encodeOneRelationshipData : Internal.RelationshipData -> Value
encodeOneRelationshipData v =
    object
        [ ( "id", string v.id )
        , ( "type", string v.type_ )
        ]


getAllIncluded : List ResourceInfo -> List ResourceInfo
getAllIncluded resources_ =
    resources_
        |> List.map (\(Internal.ResourceInfo { included }) -> included)
        |> List.concat
        |> Internal.mergeIncluded []
