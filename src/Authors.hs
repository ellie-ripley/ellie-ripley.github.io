{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Authors (Author(..), authorFile, me, cervr, authors, authorLookup, makeAuthorLink) where

import Data.Text (Text)
import qualified Data.Yaml.Aeson as Y
import qualified Data.ByteString as BS
import Lucid

import WebsiteTools

-- SECTION: Read the data

authorFile :: FilePath
authorFile = "./src/authors.yaml"

getAuthors :: IO (Either Y.ParseException [Author])
getAuthors = do
  aData <- BS.readFile authorFile
  return (Y.decodeEither' aData)

authors :: IO [Author]
authors = do
  eAuths <- getAuthors
  case eAuths of
    Left pex -> do
      putStrLn $ "ERROR: Couldn't parse authors file: " <> show authorFile
      putStrLn $ show pex
      return builtinAuthors
    Right as -> do
      putStrLn $ "Successfully parsed authors file: " <> show authorFile
      let finalAuths = builtinAuthors ++ as
      putStrLn $ "     " <> show (length finalAuths) <> " authors ready"
      return finalAuths

  
-- SECTION: built-in authors

builtinAuthors :: [Author]
builtinAuthors = [ me, pablo, paul, robert ]

me :: Author
me = Author { name = "Ellie Ripley"
            , authorUrl = "https://negation.rocks"
            , otherNames = [ "me" ]
            }

pablo :: Author
pablo = Author { name = "Pablo Cobreros"
               , authorUrl = ""
               , otherNames = [ "pabloCobreros" ]
               }

paul :: Author
paul = Author { name = "Paul Égré"
              , authorUrl = ""
              , otherNames = [ "paulEgre"
                             , "Paul Egré"
                             , "Paul Égre"
                             , "Paul Egre"
                             ]
              }

robert :: Author
robert = Author { name = "Robert van Rooij"
                , authorUrl = ""
                , otherNames = [ "robertvanRooij" ]
                }

cervr :: [Author]
cervr = [ pablo, paul, me, robert ]





authorLookup :: Text -> [Author] -> Either Text Author
authorLookup nm [] = Left nm
authorLookup nm (a@(Author{..}) : as)
  | nm == name = Right a
  | nm `elem` otherNames = Right a
  | otherwise = authorLookup nm as

makeAuthorLink :: Author -> Html ()
makeAuthorLink auth = case (authorUrl auth) of
                            "" -> toHtml (name auth)
                            x  -> a_ [href_ x, target_ "_blank"] (toHtml (name auth))
