{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeSitter.AST.Internal.Core (
  NodeTypes (..),
  TypeName (..),
  FieldName (..),
  NodeId (..),
  NodeType (..),
  ChildType (..),
) where

import Data.Aeson.KeyMap qualified as KeyMap (member)
import Data.Aeson.Types (Encoding, FromJSON (..), FromJSONKey, KeyValue ((.=)), Object, Parser, ToJSON (..), ToJSONKey, Value (..), object, pairs, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Records (HasField (..))

--------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}

--------------------------------------------------------------------------------

newtype NodeTypes = NodeTypes (Vector NodeType)
  deriving newtype (ToJSON, FromJSON)

newtype TypeName = TypeName Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype FieldName = FieldName Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data NodeId = NodeId
  { type_ :: !TypeName
  , named :: {-# UNPACK #-} !Bool
  }
  deriving stock (Eq, Ord, Show)

data NodeType
  = NodeType
      { id_ :: {-# UNPACK #-} !NodeId
      , fields :: !(Map FieldName ChildType)
      , children :: !(Maybe ChildType)
      }
  | NodeSuperType
      { id_ :: {-# UNPACK #-} !NodeId
      , subtypes :: !(Vector NodeType)
      }
  deriving stock (Show)

data ChildType = ChildType
  { required :: {-# UNPACK #-} !Bool
  , multiple :: {-# UNPACK #-} !Bool
  , types :: !(Vector NodeId)
  }
  deriving stock (Show)

-- | Support the `type_` field on `NodeType`.
instance HasField "type_" NodeType TypeName where
  getField :: NodeType -> TypeName
  getField = (.id_.type_)
  {-# INLINE getField #-}

-- | Support the `named` field on `NodeType`.
instance HasField "named" NodeType Bool where
  getField :: NodeType -> Bool
  getField = (.id_.named)
  {-# INLINE getField #-}

--------------------------------------------------------------------------------
-- FromJSON instances
--------------------------------------------------------------------------------

parseNodeId :: Object -> Parser NodeId
parseNodeId o = NodeId <$> o .: "type" <*> o .: "named"

instance FromJSON NodeId where
  parseJSON :: Value -> Parser NodeId
  parseJSON = withObject "NodeId" parseNodeId

instance FromJSON NodeType where
  parseJSON :: Value -> Parser NodeType
  parseJSON = withObject "NodeType" $ \o -> do
    id_ <- parseNodeId o
    -- Does the value have a `subtypes` field?
    if KeyMap.member "subtypes" o
      -- ... it does; it's an abstract `NodeSuperType`
      then NodeSuperType id_ <$> o .: "subtypes"
      -- ... it does not; it's a concrete `NodeType`
      else NodeType id_ <$> o .:? "fields" .!= mempty <*> o .:? "children"

instance FromJSON ChildType where
  parseJSON :: Value -> Parser ChildType
  parseJSON = withObject "ChildType" $ \o ->
    ChildType <$> o .: "required" <*> o .: "multiple" <*> o .:? "types" .!= mempty

--------------------------------------------------------------------------------
-- ToJSON instances
--------------------------------------------------------------------------------

nodeIdToKVs :: (KeyValue e kv) => NodeId -> [kv]
nodeIdToKVs = \nodeId ->
  [ "type" .= nodeId.type_
  , "named" .= nodeId.named
  ]
{-# INLINE nodeIdToKVs #-}

instance ToJSON NodeId where
  toJSON :: NodeId -> Value
  toJSON = object . nodeIdToKVs

  toEncoding :: NodeId -> Encoding
  toEncoding = pairs . mconcat . nodeIdToKVs

nodeTypeToKVs :: (KeyValue e kv) => NodeType -> [kv]
nodeTypeToKVs = \nodeType ->
  nodeIdToKVs nodeType.id_
    <> [ "fields" .= nodeType.fields
       , "children" .= nodeType.children
       ]
{-# INLINE nodeTypeToKVs #-}

instance ToJSON NodeType where
  toJSON :: NodeType -> Value
  toJSON = object . nodeTypeToKVs

  toEncoding :: NodeType -> Encoding
  toEncoding = pairs . mconcat . nodeTypeToKVs

childTypeToKVs :: (KeyValue e kv) => ChildType -> [kv]
childTypeToKVs = \childType ->
  [ "required" .= childType.required
  , "multiple" .= childType.multiple
  , "types" .= childType.types
  ]
{-# INLINE childTypeToKVs #-}

instance ToJSON ChildType where
  toJSON :: ChildType -> Value
  toJSON = object . childTypeToKVs

  toEncoding :: ChildType -> Encoding
  toEncoding = pairs . mconcat . childTypeToKVs
