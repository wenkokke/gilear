{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, optional, progDesc, short, strOption, (<**>))
import TreeSitter.GenerateAst.Internal.CodeGen (Metadata (..), generateAst)
import TreeSitter.GenerateAst.Internal.Data (toDataTypes)

template :: Text
template = T.decodeUtf8 $(embedFileRelative "data/Ast.hs.template")

data Options = Options
  { inputGrammarFile :: FilePath
  , outputFile :: Maybe FilePath
  , metadata :: Metadata
  }

parserMetadata :: Parser Metadata
parserMetadata =
  Metadata
    <$> strOption
      ( short 's'
          <> long "start-rule-name"
          <> metavar "RULE_NAME"
          <> help "The start rule name."
      )
    <*> strOption
      ( short 'm'
          <> long "output-module-name"
          <> metavar "MODULE_NAME"
          <> help "The name of the generated Haskell module."
      )

parserOptions :: Parser Options
parserOptions =
  Options
    <$> strOption
      ( short 'g'
          <> long "input-grammar-file"
          <> metavar "FILE"
          <> help "The input grammar.json file."
      )
    <*> optional
      ( strOption
          ( short 'o'
              <> long "output-file"
              <> metavar "FILE"
              <> help "The path to the generated Haskell file."
          )
      )
    <*> parserMetadata

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (parserOptions <**> helper)
    (fullDesc <> progDesc "Generate a Haskell module with an Ast for a tree-sitter grammar.")

main :: IO ()
main = do
  Options{..} <- execParser optionsInfo
  let Metadata{..} = metadata
  grammar <- either fail pure =<< eitherDecodeFileStrict inputGrammarFile
  let dataTypes = toDataTypes startRuleName grammar
  result <- either fail pure (generateAst metadata dataTypes "Ast.hs.template" template)
  maybe T.putStrLn T.writeFile outputFile result
