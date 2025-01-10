{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Authors (Author(..), authorFile, eAuthors, authorLookup, makeAuthorLink) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Yaml.Aeson as Y
import qualified Data.ByteString as BS
import Lucid

import Data.Aeson
import Data.Aeson.TH
import Data.Text.Array (new)

data Author = Author { name :: Text
                     , authorUrl :: Text
                     , otherNames :: [Text]
                     } deriving (Show, Eq)

deriveJSON defaultOptions ''Author

-- SECTION: Read the data

authorFile :: FilePath
authorFile = "./src/authors.yaml"

eAuthors :: IO (Either Y.ParseException [Author])
eAuthors = do
  aData <- BS.readFile authorFile
  return (Y.decodeEither' aData)

authorLookup :: Text -> [Author] -> Maybe Author
authorLookup _ [] = Nothing
authorLookup nm (a@(Author{..}) : as)
  | nm == name = Just a
  | nm `elem` otherNames = Just a
  | otherwise = authorLookup nm as

makeAuthorLink :: [Author] -> Text -> Html ()
makeAuthorLink amp tg =
  case authorLookup tg amp of
    Nothing -> toHtml $ "Error: " <> tg
    Just auth -> case (authorUrl auth) of
                      "" -> toHtml (name auth)
                      x  -> a_ [href_ x, target_ "_blank"] (toHtml (name auth))
