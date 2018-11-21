module JsonApi.Internal.ResourceInfo exposing (OneOrManyRelationships(..), OneOrManyRelationshipsInternal(..), OneOrMoreRelationshipData(..), Relationship, RelationshipData, ResourceInfo(..), ResourceInfoInternal, addOrUpdateResource, addRelationship, build, includedFromResources, isGoodRelationship, mergeIncluded, relationshipToData, resourceInfoToData, updateIncluded)

import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import List.Extra


type ResourceInfo
    = ResourceInfo ResourceInfoInternal


type alias ResourceInfoInternal =
    { included : List ResourceInfo
    , id : Maybe String
    , links : Dict String String
    , type_ : String
    , relationships : Dict String Relationship
    , attributes : Value
    }


type OneOrMoreRelationshipData
    = NoRelationship
    | One RelationshipData
    | Many (List RelationshipData)


type alias RelationshipData =
    { id : String
    , type_ : String
    }


type alias Relationship =
    { data : OneOrMoreRelationshipData
    , links : Dict String String
    }


type OneOrManyRelationships
    = OneOrManyRelationships OneOrManyRelationshipsInternal


type OneOrManyRelationshipsInternal
    = OneRelationship ResourceInfo
    | ManyRelationships (List ResourceInfo)


build : String -> ResourceInfoInternal
build type_ =
    { included = []
    , id = Nothing
    , links = Dict.empty
    , type_ = type_
    , relationships = Dict.empty
    , attributes = object []
    }


addRelationship : String -> OneOrManyRelationshipsInternal -> ResourceInfoInternal -> ResourceInfoInternal
addRelationship type_ oneOrMoreRelationships info =
    { info | relationships = Dict.insert type_ (Relationship (relationshipToData oneOrMoreRelationships) Dict.empty) info.relationships }
        |> updateIncluded oneOrMoreRelationships


relationshipToData : OneOrManyRelationshipsInternal -> OneOrMoreRelationshipData
relationshipToData oneOrMoreRelationships =
    case oneOrMoreRelationships of
        OneRelationship resourceInfo ->
            resourceInfo |> resourceInfoToData |> One

        ManyRelationships resourceInfoList ->
            List.map resourceInfoToData resourceInfoList |> Many


resourceInfoToData : ResourceInfo -> RelationshipData
resourceInfoToData (ResourceInfo resourceInfo) =
    { id = resourceInfo.id |> Maybe.withDefault "", type_ = resourceInfo.type_ }


updateIncluded : OneOrManyRelationshipsInternal -> ResourceInfoInternal -> ResourceInfoInternal
updateIncluded oneOrManyRelationshipsInternal info =
    { info | included = mergeIncluded info.included (includedFromResources oneOrManyRelationshipsInternal) }


includedFromResources : OneOrManyRelationshipsInternal -> List ResourceInfo
includedFromResources oneOrManyRelationships =
    let
        getResourceAndIncluded : ResourceInfo -> List ResourceInfo
        getResourceAndIncluded (ResourceInfo resourceInfo) =
            ResourceInfo resourceInfo :: resourceInfo.included
    in
    case oneOrManyRelationships of
        OneRelationship resourceInfo ->
            getResourceAndIncluded resourceInfo

        ManyRelationships resourceInfoList ->
            List.map getResourceAndIncluded resourceInfoList
                |> List.concat


mergeIncluded : List ResourceInfo -> List ResourceInfo -> List ResourceInfo
mergeIncluded =
    List.foldl addOrUpdateResource


addOrUpdateResource : ResourceInfo -> List ResourceInfo -> List ResourceInfo
addOrUpdateResource res resources =
    case List.Extra.findIndex (isGoodRelationship res) resources of
        Nothing ->
            res :: resources

        Just idx ->
            List.Extra.setAt idx res resources


isGoodRelationship : ResourceInfo -> ResourceInfo -> Bool
isGoodRelationship (ResourceInfo res1) (ResourceInfo res2) =
    res1.id == res2.id && res1.type_ == res2.type_
