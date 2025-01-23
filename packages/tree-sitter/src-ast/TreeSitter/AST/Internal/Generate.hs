module TreeSitter.AST.Internal.Generate where

-- import Data.Map (Map)
-- import Data.Map qualified as M
-- import Data.Maybe (catMaybes)
-- import Data.Vector (Vector)
-- import Data.Vector qualified as V

-- data ArgDesc = ArgDesc
--   { name :: Maybe FieldName
--   , type_ :: TypeDesc
--   }

-- data ConDesc = ConDesc
--   { name :: TypeName
--   , args :: Vector ArgDesc
--   }

-- data DataDesc = DataDesc
--   { name :: TypeName
--   , cons :: Vector ConDesc
--   }

-- data TypeDesc
--   = Name TypeName
--   | NonEmpty TypeDesc
--   | List TypeDesc
--   | Maybe TypeDesc
--   | Either TypeDesc TypeDesc

-- fromNodeTypes :: NodeTypes -> Vector DataDesc
-- fromNodeTypes (NodeTypes nodeTypes) = _
--  where
--   conDesc :: Map TypeName ConDesc
--   conDesc = M.fromList . catMaybes $ do
--     V.toList nodeTypes >>= \case
--       NodeSuperType{} -> pure Nothing
--       NodeType{id_ = NodeId{..}, ..}
--         | null fields && null children -> do
--             pure $ Just (type_, ConDesc type_ mempty)
--         | null fields -> do
--             _
--         | null children -> do
--             _
--         | otherwise -> do
--             _

-- generate :: NodeTypes -> ByteString
-- generate nodeTypes = _

-- case (required, multiple) of
--   (True,  True)  -> NonEmpty
--   (False, True)  -> List
--   (True,  False) -> Identity
--   (False, False) -> Maybe
