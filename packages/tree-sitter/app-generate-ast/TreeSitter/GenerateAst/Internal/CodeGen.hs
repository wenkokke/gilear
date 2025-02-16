{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TreeSitter.GenerateAst.Internal.CodeGen (
  Metadata (..),
  generateAst,
) where

import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Functor.Identity (Identity (..))
import Data.List (uncons)
import Data.Map qualified as M
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Text.DocLayout (Doc, render)
import Text.DocLayout qualified as Doc (Doc (..))
import Text.DocTemplates (Context (..), ToContext (..), Val (..), applyTemplate)
import TreeSitter.GenerateAst.Internal.Data (Constr (..), Data (..), Field (..), Name (..), Type (..), fieldName, toDataTypes)
import TreeSitter.GenerateAst.Internal.Grammar (RuleName, Grammar (..))
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- Template Parser and Renderer
--------------------------------------------------------------------------------

data Metadata = Metadata
  { startRuleName :: RuleName
  , moduleName :: Maybe Text
  , pretty :: Bool
  }

generateAst :: Metadata -> Grammar -> FilePath -> Text -> Either String Text
generateAst Metadata{..} grammar templateFile template = errorOrModule
  where
    defaultModuleName = snakeToCase Upper grammar.name <> "Ast"
    metadataContext = Context . M.fromList $
      [ ("startRuleName", textToVal startRuleName)
      , ("moduleName", textToVal $ fromMaybe defaultModuleName moduleName)
      , ("pretty", BoolVal pretty)
      ]
    dataTypesContext =
      Context . M.fromList $
        [ ("dataTypes", toVal dataTypes)
        , ("startSort", maybe NullVal (\(Data name _, _) -> toVal name) (uncons dataTypes))
        ]
    dataTypes = toDataTypes startRuleName grammar
    context = metadataContext <> dataTypesContext
    errorOrModule = renderTemplate templateFile template context

data RenderState = InText | InTemplate [Text]

renderTemplate :: (ToContext Text b) => FilePath -> Text -> b -> Either String Text
renderTemplate templateFile templateText context =
  fmap (TL.toStrict . TLB.toLazyText) . go InText . zip [1 ..] . T.lines $ templateText
 where
  go :: RenderState -> [(Int, Text)] -> Either String TLB.Builder
  go InText [] = pure mempty
  go InText ((_n, ln) : lns)
    | ln == "{-+}" = go (InTemplate mempty) lns
    | otherwise = (line ln <>) <$> go InText lns
  go (InTemplate acc) [] = template acc
  go (InTemplate acc) ((_n, ln) : lns)
    | ln == "{+-}" = (<>) <$> template acc <*> go InText lns
    | otherwise = go (InTemplate (ln : acc)) lns

  line :: Text -> TLB.Builder
  line ln = TLB.fromText (ln <> "\n")

  template :: [Text] -> Either String TLB.Builder
  template acc = do
    let tpl = T.unlines . reverse $ acc
    (doc :: Doc Text) <- runIdentity (applyTemplate templateFile tpl context)
    pure . TLB.fromText . render Nothing $ doc

textToVal :: Text -> Val Text
textToVal text = SimpleVal (Doc.Text (T.length text) text)

isNodeLike :: Type -> Bool
isNodeLike = \case
  Type _name -> True
  List a -> isNodeLike a
  NonEmpty a -> isNodeLike a
  Unit -> False
  Tuple a b -> isNodeLike a && isNodeLike b
  Either a b -> isNodeLike a && isNodeLike b
  Maybe a -> isNodeLike a

instance ToContext Text Data where
  toVal :: Data -> Val Text
  toVal = MapVal . toContext
  toContext :: Data -> Context Text
  toContext Data{..} =
    Context . M.fromList $
      [ ("name", toVal name)
      , ("constrs", ListVal (toVal . (name,) <$> constrs))
      ]

instance ToContext Text (Name, Constr) where
  toVal :: (Name, Constr) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Name, Constr) -> Context Text
  toContext (sortName, Constr{..}) =
    Context . M.fromList $
      [ ("sort", toVal sortName)
      , ("name", toVal name)
      , ("hasChildren", toVal hasChildren)
      , ("fields", ListVal (toVal <$> zip [(0 :: Int) ..] fields))
      ]
   where
    hasChildren :: Bool
    hasChildren = any (isNodeLike . type_) fields

instance ToContext Text (Int, Field) where
  toVal :: (Int, Field) -> Val Text
  toVal = MapVal . toContext
  toContext :: (Int, Field) -> Context Text
  toContext (index, field) =
    Context . M.fromList $
      [ ("name", maybe NullVal toVal (fieldName field))
      , ("type", toVal (type_ field))
      , ("index", SimpleVal . fromString . show $ index)
      ]

instance ToContext Text Type where
  toVal :: Type -> Val Text
  toVal = textToVal . TL.toStrict . TLB.toLazyText . t2t False
   where
    par b t = if b then "(" <> t <> ")" else t
    t2t p = \case
      Type name -> par p ("Node" <> " " <> TLB.fromText (snakeToCase Upper (unName name) <> "Sort"))
      List a -> "[" <> t2t False a <> "]"
      NonEmpty a -> par p ("NonEmpty" <> " " <> t2t True a)
      Unit -> "()"
      Tuple a b -> "(" <> t2t False a <> ", " <> t2t False b <> ")"
      Either a b -> par p ("Either" <> " " <> t2t True a <> " " <> t2t True b)
      Maybe a -> par p ("Maybe" <> " " <> t2t True a)

instance ToContext Text Name where
  toVal :: Name -> Val Text
  toVal name =
    MapVal . Context . M.fromList $
      [ ("text", textToVal (unName name))
      , ("camelCase", textToVal (snakeToCase Upper (unName name)))
      ]

--------------------------------------------------------------------------------
-- Helper functions for case conversion
--------------------------------------------------------------------------------

data Case = Upper | Lower | Keep

-- | @`snakeToCase` c txt@ converts a string from snake or kebab case to camel case.
snakeToCase :: Case -> Text -> Text
snakeToCase = \b -> T.pack . go b . T.unpack
 where
  go _ [] = []
  go b (c : cs) = if isAlphaNum c then c `to` b : go Keep cs else go Upper cs

  to c Upper = toUpper c
  to c Lower = toLower c
  to c Keep = c
