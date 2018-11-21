module JsonApi.Data.ResourcePayloads exposing (commentWithoutIncluded, dataIsList, invalidPayloadCommentsIsOneElement, invalidPayloadCreatorIsList, invalidPayloadWithoutAttributes, invalidPayloadWithoutCreatorAttributes, invalidPayloadWithoutCreatorId, invalidPayloadWithoutCreatorRelationships, invalidPayloadWithoutCreatorType, invalidPayloadWithoutData, invalidPayloadWithoutId, invalidPayloadWithoutRelationshipIdNotFound, invalidPayloadWithoutRelationshipInIncluded, invalidPayloadWithoutRelationshipInRelationships, invalidPayloadWithoutRelationshipTypeNotFound, invalidPayloadWithoutRelationships, invalidPayloadWithoutType, validPayload, validPayloadWithoutLinks)


validPayload : String
validPayload =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/1"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


validPayloadWithoutLinks : String
validPayloadWithoutLinks =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutData : String
invalidPayloadWithoutData =
    """
    {
        "data-bad": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutAttributes : String
invalidPayloadWithoutAttributes =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes-bad": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutId : String
invalidPayloadWithoutId =
    """
    {
        "data": {
            "type": "posts",
            "id-bad": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutType : String
invalidPayloadWithoutType =
    """
    {
        "data": {
            "type-bad": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutRelationshipInRelationships : String
invalidPayloadWithoutRelationshipInRelationships =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator-bad": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutRelationshipIdNotFound : String
invalidPayloadWithoutRelationshipIdNotFound =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "bad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutRelationshipTypeNotFound : String
invalidPayloadWithoutRelationshipTypeNotFound =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators-bad",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutRelationshipInIncluded : String
invalidPayloadWithoutRelationshipInIncluded =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators-bad",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutRelationships : String
invalidPayloadWithoutRelationships =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships-bad": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutCreatorAttributes : String
invalidPayloadWithoutCreatorAttributes =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes-bad": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutCreatorId : String
invalidPayloadWithoutCreatorId =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id-bad": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutCreatorType : String
invalidPayloadWithoutCreatorType =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type-bad": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadWithoutCreatorRelationships : String
invalidPayloadWithoutCreatorRelationships =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships-bad": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadCreatorIsList : String
invalidPayloadCreatorIsList =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": [
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    ],
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": [
                        {
                            "type": "comment",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                        },
                        {
                            "type": "comment",
                            "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                        }
                    ]
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


invalidPayloadCommentsIsOneElement : String
invalidPayloadCommentsIsOneElement =
    """
    {
        "data": {
            "type": "posts",
            "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
            "links": {
                "self": "http://link-to-post/2"
            },
            "attributes": {
                "title": "First post",
                "content": "First post content"
            },
            "relationships": {
                "creator": {
                    "data": {
                        "type": "creators",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                    },
                    "links": {
                        "related": "http://link-to-creator/1"
                    }
                },
                "comments": {
                    "links": {},
                    "data": {
                        "type": "comment",
                        "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                    }
                }
            }
        },
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


dataIsList : String
dataIsList =
    """
    {
        "data": [
            {
                "type": "posts",
                "id": "13608770-76dd-47e5-a1c4-4d0d9c2483ad",
                "links": {
                    "self": "http://link-to-post/2"
                },
                "attributes": {
                    "title": "First post",
                    "content": "First post content"
                },
                "relationships": {
                    "creator": {
                        "data": {
                            "type": "creators",
                            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad"
                        },
                        "links": {
                            "related": "http://link-to-creator/1"
                        }
                    },
                    "comments": {
                        "links": {},
                        "data": [
                            {
                                "type": "comment",
                                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab"
                            },
                            {
                                "type": "comment",
                                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f"
                            }
                        ]
                    }
                }
            }
        ],
        "included": [
            {
                "type": "creators",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ad",
                "attributes": {
                    "firstname": "John",
                    "lastname": "Doe"
                },
                "links": {
                    "self": "http://link-to-creator/1"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
                "attributes": {
                    "content": "Comment 2 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/2"
                },
                "relationships": {}
            },
            {
                "type": "comment",
                "id": "cb0759b0-03ab-4291-b067-84a9017fea6f",
                "attributes": {
                    "content": "Comment 3 content",
                    "email": "john@doe.com"
                },
                "links": {
                    "self": "http://link-to-comment/3"
                },
                "relationships": {}
            }
        ]
    }
    """


commentWithoutIncluded : String
commentWithoutIncluded =
    """
    {
        "data": {
            "type": "comments",
            "id": "22208770-76dd-47e5-a1c4-4d0d9c2483ab",
            "attributes": {
                "content": "Comment content",
                "email": "john@doe.com"
            },
            "links": {
                "self": "http://link-to-comment/2"
            },
            "relationships": {}
        }
    }
    """
