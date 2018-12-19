module JsonApi.Internal.Document exposing
    ( Document(..)
    , DocumentEncode(..)
    , DocumentEncodeInternal
    , DocumentInternal
    , NoData(..)
    , NoMeta(..)
    )

import Json.Encode exposing (Value)
import JsonApi.Internal.ResourceInfo as Internal


type NoMeta
    = NoMeta


type NoData
    = NoData


type Document meta data
    = Document (DocumentInternal meta data)


type alias DocumentInternal meta data =
    { jsonApiVersion : String
    , meta : meta
    , data : data
    }


type DocumentEncode
    = DocumentEncode DocumentEncodeInternal


type alias DocumentEncodeInternal =
    { jsonApiVersion : String
    , meta : Maybe Value
    , data : Maybe Internal.OneOrManyRelationshipsInternal
    }
