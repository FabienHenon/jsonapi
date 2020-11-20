module JsonApi.Data.Posts exposing (Comment, Creator, EmptyComment, EmptyPost, Post, PostWithRelationshipDesc, badCreatorDecoder, badPostDecoder, comment1, comment2, comment3, commentDecoder, creator, creatorDecoder, emptyPostDecoder, fakeUser, metaDecoder, post2, postBadCreatorDecoder, postDecoder, postDecoderWithRelationshipsDesc, postDecoderWithoutCreator, postNoLink, postWithoutAttributesDecoder, postWithoutRelationshipsDecoder, posts)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, bool, decodeString, field, map, map2, map4, map5, map6, oneOf, string, succeed)
import JsonApi.Decode as Decode
import JsonApi.Resource exposing (Resource)


type alias Post =
    { id : String
    , links : Dict String String
    , title : String
    , content : String
    , creator : Creator
    , comments : List Comment
    }


type alias EmptyPost =
    { id : String
    , links : Dict String String
    , creator : Creator
    , comments : List EmptyComment
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


type alias EmptyComment =
    { id : String
    , links : Dict String String
    }


type alias Meta =
    { redirect : Bool
    }


type alias PostWithRelationshipDesc =
    { id : String
    , links : Dict String String
    , title : String
    , content : String
    , relationships : Dict String JsonApi.Resource.RelationshipDesc
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


commentDecoder : Resource -> Decoder Comment
commentDecoder resourceInfo =
    map4 Comment
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "content" string)
        (field "email" string)


emptyCommentDecoder : Resource -> Decoder EmptyComment
emptyCommentDecoder resourceInfo =
    map2 EmptyComment
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))


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


emptyPostDecoder : Resource -> Decoder EmptyPost
emptyPostDecoder resourceInfo =
    map4 EmptyPost
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo emptyCommentDecoder)


postDecoderWithoutCreator : Resource -> Decoder Post
postDecoderWithoutCreator resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (oneOf [ Decode.relationship "creator" resourceInfo creatorDecoder, succeed fakeUser ])
        (Decode.relationships "comments" resourceInfo commentDecoder)


postDecoderWithRelationshipsDesc : Resource -> Decoder PostWithRelationshipDesc
postDecoderWithRelationshipsDesc resourceInfo =
    map5 PostWithRelationshipDesc
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (succeed (JsonApi.Resource.getRelationshipsDesc resourceInfo))


fakeUser : Creator
fakeUser =
    { id = "creator-1"
    , links = Dict.fromList [ ( "self", "http://url-to-creator/1" ) ]
    , firstname = "Fake"
    , lastname = "Fake"
    }


badCreatorDecoder : Resource -> Decoder Creator
badCreatorDecoder resourceInfo =
    map4 Creator
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "bad" string)
        (field "lastname" string)


postBadCreatorDecoder : Resource -> Decoder Post
postBadCreatorDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo badCreatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


postWithoutRelationshipsDecoder : Resource -> Decoder Post
postWithoutRelationshipsDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "title" string)
        (field "content" string)
        (succeed { id = "test", links = Dict.empty, firstname = "John", lastname = "Doe" })
        (succeed [])


postWithoutAttributesDecoder : Resource -> Decoder Post
postWithoutAttributesDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (succeed "fake title")
        (succeed "fake content")
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


badPostDecoder : Resource -> Decoder Post
badPostDecoder resourceInfo =
    map6 Post
        (succeed (JsonApi.Resource.id resourceInfo))
        (succeed (JsonApi.Resource.links resourceInfo))
        (field "bad" string)
        (field "content" string)
        (Decode.relationship "creator" resourceInfo creatorDecoder)
        (Decode.relationships "comments" resourceInfo commentDecoder)


metaDecoder : Decoder Meta
metaDecoder =
    map Meta
        (field "redirect" bool)
