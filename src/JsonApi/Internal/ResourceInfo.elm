module JsonApi.Internal.ResourceInfo exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Value)


type ResourceInfo
    = ResourceInfo ResourceInfoInternal


type alias ResourceInfoInternal =
    { included : List ResourceInfo
    , id : String
    , links : Dict String String
    , type_ : String
    , relationships : Dict String Relationship
    , attributes : Value
    }


type OneOrMoreRelationshipData
    = One RelationshipData
    | Many (List RelationshipData)


type alias RelationshipData =
    { id : String
    , type_ : String
    }


type alias Relationship =
    { data : OneOrMoreRelationshipData
    , links : Dict String String
    }
