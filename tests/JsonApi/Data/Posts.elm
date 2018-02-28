module JsonApi.Data.Posts exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, field, string, succeed, decodeString, map4, map6)
import JsonApi.Decode as Decode
import JsonApi exposing (ResourceInfo)


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


commentDecoder : ResourceInfo -> Decoder Comment
commentDecoder resourceInfo =
    map4 Comment
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "content" string)
        (field "email" string)


creatorDecoder : ResourceInfo -> Decoder Creator
creatorDecoder resourceInfo =
    map4 Creator
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "firstname" string)
        (field "lastname" string)


postDecoder : ResourceInfo -> Decoder Post
postDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


badCreatorDecoder : ResourceInfo -> Decoder Creator
badCreatorDecoder resourceInfo =
    map4 Creator
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "bad" string)
        (field "lastname" string)


postBadCreatorDecoder : ResourceInfo -> Decoder Post
postBadCreatorDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo badCreatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


postWithoutRelationshipsDecoder : ResourceInfo -> Decoder Post
postWithoutRelationshipsDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (succeed { id = "test", links = Dict.empty, firstname = "John", lastname = "Doe" })
        (succeed [])


postWithoutAttributesDecoder : ResourceInfo -> Decoder Post
postWithoutAttributesDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (succeed "fake title")
        (succeed "fake content")
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


badPostDecoder : ResourceInfo -> Decoder Post
badPostDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.id resourceInfo))
        (succeed (JsonApi.links resourceInfo))
        (field "bad" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)
