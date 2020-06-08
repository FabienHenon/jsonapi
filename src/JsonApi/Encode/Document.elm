module JsonApi.Encode.Document exposing
    ( Document
    , build
    , jsonApiVersion, meta, resource, resources, links
    , withJsonApiVersion, withMeta, withResource, withResources, withLinks
    )

{-| Provides a type alias and functions to create a new Json API document to be
encoded via `JsonApi.Encode.document`.


# Type

@docs Document


# New document

@docs build


# Getter functions

@docs jsonApiVersion, meta, resource, resources, links


# Setter functions

@docs withJsonApiVersion, withMeta, withResource, withResources, withLinks

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import JsonApi.Internal.Document as Internal
import JsonApi.Internal.ResourceInfo as ResInternal
import JsonApi.Resource exposing (Resource)


{-| The `Document` represents the global Json API document.
This version of the `Document` is to be encoded via `JsonApi.Encode.document`. Thus
it has functions to create a new `Document`.

_Example of json api document:_

```json
{
    "data": [
        // resources ...
    ],
    "meta": {
        "redirect": true
    },
    "jsonapi": {
        "version": "1.0"
    }
}
```

And how to build it with the `JsonApi.Encode.Document` module:

    build
        |> withJsonApiVersion "1.0"
        |> withMeta (object [ ( "redirect", bool True ) ])
        |> withResources resources

-}
type alias Document =
    Internal.DocumentEncode


{-| Creates a new Json API `Document` with no meta, no resource and a default
Json API version set to 1.0

    myDocument : Document
    myDocument =
        build

-}
build : Document
build =
    Internal.DocumentEncode
        { jsonApiVersion = "1.0"
        , meta = Nothing
        , data = Nothing
        , links = Dict.empty
        }


{-| Retrieves the Json API version.

From the json document above, `jsonApiVersion` will return `"1.0"`

-}
jsonApiVersion : Document -> String
jsonApiVersion (Internal.DocumentEncode doc) =
    doc.jsonApiVersion


{-| Sets the Json API version of the document

    myDocument : Document
    myDocument =
        build
            |> withJsonApiVersion "1.0"

\_The default version is 1.0

-}
withJsonApiVersion : String -> Document -> Document
withJsonApiVersion jsonApiVersion_ (Internal.DocumentEncode doc) =
    Internal.DocumentEncode { doc | jsonApiVersion = jsonApiVersion_ }


{-| Retrieves the `meta` object of the `Document` if it exists.
-}
meta : Document -> Maybe Value
meta (Internal.DocumentEncode doc) =
    doc.meta


{-| Retrieves the `links` object of the `Document`.
-}
links : Document -> Dict String String
links (Internal.DocumentEncode doc) =
    doc.links


{-| Sets the `meta` value of the `Document`

    myDocument : Document
    myDocument =
        build
            |> withMeta (object [ ( "redirect", bool True ) ])

-}
withMeta : Value -> Document -> Document
withMeta meta_ (Internal.DocumentEncode doc) =
    Internal.DocumentEncode { doc | meta = Just meta_ }


{-| Sets the `links` value of the `Document`

    myDocument : Document
    myDocument =
        build
            |> withLinks (Dict.fromList [ ( "self", "http://self" ) ])

-}
withLinks : Dict String String -> Document -> Document
withLinks links_ (Internal.DocumentEncode doc) =
    Internal.DocumentEncode { doc | links = links_ }


{-| Retrieves the resource (in the `data` object) of the `Document`.
If there is no resource `Nothing` is returned.
If there are several resources, the first resource is returned.
-}
resource : Document -> Maybe Resource
resource (Internal.DocumentEncode doc) =
    doc.data
        |> Maybe.andThen
            (\data ->
                case data of
                    ResInternal.OneRelationship res ->
                        Just res

                    ResInternal.ManyRelationships (res :: _) ->
                        Just res

                    ResInternal.ManyRelationships [] ->
                        Nothing
            )


{-| Retrieves the resources (in the `data` array) of the `Document`.
If there is no resource set `Nothing` is returned.
If resources have been set but the list is empty, an empty list is also returned.
If there is only a resouce, a list with this resource is returned.
-}
resources : Document -> Maybe (List Resource)
resources (Internal.DocumentEncode doc) =
    doc.data
        |> Maybe.map
            (\data ->
                case data of
                    ResInternal.OneRelationship res ->
                        [ res ]

                    ResInternal.ManyRelationships resList ->
                        resList
            )


{-| Sets the `data` value of the Json API `Document` to one resource.

    myResource : Resource
    myResource =
        Resource.build "posts"
            |> Resource.withId "post-1"

    myDocument : Document
    myDocument =
        build
            |> withResource myResource

-}
withResource : Resource -> Document -> Document
withResource res (Internal.DocumentEncode doc) =
    Internal.DocumentEncode { doc | data = Just (ResInternal.OneRelationship res) }


{-| Sets the `data` value of the Json API `Document` to a list of resources.

    myResources : List Resource
    myResources =
        [ Resource.build "posts"
            |> Resource.withId "post-1"
        , Resource.build "posts"
            |> Resource.withId "post-2"
        ]

    myDocument : Document
    myDocument =
        build
            |> withResources myResources

-}
withResources : List Resource -> Document -> Document
withResources resList (Internal.DocumentEncode doc) =
    Internal.DocumentEncode { doc | data = Just (ResInternal.ManyRelationships resList) }
