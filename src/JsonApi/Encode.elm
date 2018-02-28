module JsonApi.Encode exposing (resources, resource)

{-| Doc


# Encoders

@docs resource, resources

-}

import JsonApi exposing (ResourceInfo, OneOrManyRelationships)
import Json.Encode exposing (Value, object, list, string)
import JsonApi.Internal.ResourceInfo as Internal
import Dict exposing (Dict)


{-| TODO
-}
resources : List ResourceInfo -> Value
resources resources =
    encodeBasePayload (getAllIncluded resources) (encodeResources resources)


{-| TODO
-}
resource : ResourceInfo -> Value
resource (Internal.ResourceInfo resource) =
    encodeBasePayload resource.included (encodeResource (Internal.ResourceInfo resource))



-- LOGIC


encodeBasePayload : List ResourceInfo -> Value -> Value
encodeBasePayload included data =
    object
        [ ( "data", data )
        , ( "included", list (List.map encodeResource included) )
        ]


encodeResources : List ResourceInfo -> Value
encodeResources resources =
    list (List.map encodeResource resources)


encodeResource : ResourceInfo -> Value
encodeResource (Internal.ResourceInfo { id, type_, attributes, relationships, links }) =
    object
        ((encodeOptionalId id)
            ++ (encodeOptionalLinks links)
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
        ((encodeOptionalLinks relationship.links)
            ++ [ ( "data", encodeRelationshipOneOrMoreData relationship.data )
               ]
        )


encodeRelationshipOneOrMoreData : Internal.OneOrMoreRelationshipData -> Value
encodeRelationshipOneOrMoreData data =
    case data of
        Internal.One d ->
            encodeOneRelationshipData d

        Internal.Many d ->
            list (List.map encodeOneRelationshipData d)


encodeOneRelationshipData : Internal.RelationshipData -> Value
encodeOneRelationshipData v =
    object
        [ ( "id", string v.id )
        , ( "type", string v.type_ )
        ]


getAllIncluded : List ResourceInfo -> List ResourceInfo
getAllIncluded resources =
    resources
        |> List.map (\(Internal.ResourceInfo { included }) -> included)
        |> List.concat
        |> Internal.mergeIncluded []
