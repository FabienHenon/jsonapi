module JsonApi.Encode exposing (document)

{-| Provides a function to encode documents to a `Json.Encode.Value`. You can then finally
encode it to a json api string with `Json.Encode.encode`.


# Encoders

@docs document

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value, list, null, object, string)
import JsonApi.Encode.Document exposing (Document)
import JsonApi.Internal.Document as DocInternal
import JsonApi.Internal.ResourceInfo as Internal
import JsonApi.Resource exposing (OneOrManyRelationships, Resource)


{-| Encodes a document.

Here is an example with many resources and a `meta` object:

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
    creatorToResource creator =
        JsonApi.Resource.build "creators"
            |> JsonApi.Resource.withId creator.id
            |> JsonApi.Resource.withLinks creator.links
            |> JsonApi.Resource.withAttributes
                [ ( "firstname", string creator.firstname )
                , ( "lastname", string creator.lastname )
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

    documentToEncode : Document
    documentToEncode =
        JsonApi.Encode.Document.build
            |> JsonApi.Encode.Document.withResource (postToResource post)
            |> JsonApi.Encode.Document.withMeta (object [ ( "redirect", bool True ) ])


    -- Encodes the document
    JsonApi.Encode.document documentToEncode

-}
document : Document -> Value
document (DocInternal.DocumentEncode doc) =
    doc.data
        |> Maybe.map
            (\data ->
                case data of
                    Internal.OneRelationship res ->
                        resource res

                    Internal.ManyRelationships res ->
                        resources res
            )
        |> encodeBasePayload doc


resources : List Resource -> ( List Resource, Value )
resources resources_ =
    ( getAllIncluded resources_, encodeResources resources_ )


resource : Resource -> ( List Resource, Value )
resource (Internal.ResourceInfo resource_) =
    ( resource_.included, encodeResource (Internal.ResourceInfo resource_) )



-- LOGIC


encodeBasePayload : DocInternal.DocumentEncodeInternal -> Maybe ( List Resource, Value ) -> Value
encodeBasePayload doc res =
    object
        (encodeOptionalResource res
            ++ encodeOptionalMeta doc
            ++ [ ( "jsonapi", encodeJsonApi doc ) ]
        )


encodeJsonApi : DocInternal.DocumentEncodeInternal -> Value
encodeJsonApi doc =
    object [ ( "version", string doc.jsonApiVersion ) ]


encodeOptionalMeta : DocInternal.DocumentEncodeInternal -> List ( String, Value )
encodeOptionalMeta =
    .meta >> Maybe.map (\meta -> [ ( "meta", meta ) ]) >> Maybe.withDefault []


encodeOptionalResource : Maybe ( List Resource, Value ) -> List ( String, Value )
encodeOptionalResource =
    Maybe.map
        (\( included, data ) ->
            [ ( "data", data )
            , ( "included", list encodeResource included )
            ]
        )
        >> Maybe.withDefault []


encodeResources : List Resource -> Value
encodeResources resources_ =
    list encodeResource resources_


encodeResource : Resource -> Value
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


getAllIncluded : List Resource -> List Resource
getAllIncluded resources_ =
    resources_
        |> List.map (\(Internal.ResourceInfo { included }) -> included)
        |> List.concat
        |> Internal.mergeIncluded []
