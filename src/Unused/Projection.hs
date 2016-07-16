{-# LANGUAGE OverloadedStrings #-}

module Unused.Projection where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Unused.Projection.Transform

data ParsedTransform = ParsedTransform
    { ptPre :: Text
    , ptTransforms :: [Transform]
    , ptPost :: Text
    }

translate :: Text -> Text -> Text
translate template term =
    case parseTransform template of
        Right pt -> applyTransform pt term
        Left _ -> term

applyTransform :: ParsedTransform -> Text -> Text
applyTransform pt t =
    ptPre pt
    <> runTransformations t (ptTransforms pt)
    <> ptPost pt

parseTransform :: Text -> Either String ParsedTransform
parseTransform = parseOnly parsedTransformParser

parsedTransformParser :: Parser ParsedTransform
parsedTransformParser =
    ParsedTransform
    <$> preTransformsParser
    <*> transformsParser
    <*> postTransformsParser

preTransformsParser :: Parser Text
preTransformsParser = T.pack <$> manyTill anyChar (char '{')

transformsParser :: Parser [Transform]
transformsParser = transformParser `sepBy` char '|' <* char '}'

postTransformsParser :: Parser Text
postTransformsParser = takeTill isEndOfLine

transformParser :: Parser Transform
transformParser = do
    result <- string "camelcase" <|> string "snakecase"
    return $ case result of
        "camelcase" -> Camelcase
        "snakecase" -> Snakecase
        _ -> Noop
