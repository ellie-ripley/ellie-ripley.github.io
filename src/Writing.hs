{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Writing (pieces, WritingPiece(..), wpAuthorTags, wpVenue, wpBibtex) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)
import Lucid
import Data.Monoid ((<>), mempty, mconcat)
import Data.List (intersperse)

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Yaml.Aeson as Y

import qualified Data.ByteString as BS

import Authors (Author(..), authors)
import WebsiteTools (AuthorCat(..), doiToLink, sHtml)

data YamlTest = YT { str :: Text } deriving (Eq, Show)

data WritingPiece =
      Paper { title :: Text
            , authorCat :: AuthorCat
            , writingUrl :: Text
            , bibtag :: Text
            , abstract :: Maybe Text
            , year :: Maybe Int
            , startPage :: Maybe Int
            , endPage :: Maybe Int
            , journal :: Text
            , volume :: Maybe Int
            , number :: Maybe Int
            , doi :: Maybe Text
            }
   |  Chapter { title :: Text
              , authorCat :: AuthorCat
              , writingUrl :: Text
              , bibtag :: Text
              , abstract :: Maybe Text
              , year :: Maybe Int
              , startPage :: Maybe Int
              , endPage :: Maybe Int
              , booktitle :: Text
              , editor :: [Text]
              , publisher :: Text
              }
   |  Book { title :: Text
           , authorCat :: AuthorCat
           , writingUrl :: Text
           , bibtag :: Text
           , abstract :: Maybe Text
           , year :: Maybe Int
           , publisher :: Text
           }
   deriving (Show, Eq)


deriveJSON defaultOptions ''WritingPiece
deriveJSON defaultOptions ''YamlTest


--Accessors:

yrSpEp :: WritingPiece -> Maybe (Int, Int, Int)
yrSpEp (Paper{..}) = case (catMaybes [year, startPage, endPage]) of
  [yr, sp, ep] -> Just (yr, sp, ep)
  _            -> Nothing
yrSpEp (Chapter{..}) = case (catMaybes [year, startPage, endPage]) of
  [yr, sp, ep] -> Just (yr, sp, ep)
  _            -> Nothing
yrSpEp (Book{..}) = Nothing

getAuth :: Text -> Author
getAuth tg = fromJust (M.lookup tg authors)

wpAuthorTags :: WritingPiece -> [Text]
wpAuthorTags p = case authorCat p of
                      Solo     -> ["me"]
                      CERvR    -> [ "pabloCobreros"
                                  , "paulEgre"
                                  , "me"
                                  , "robertVanRooij"
                                  ]
                      Other as -> as

wpVenue :: WritingPiece -> Html ()
wpVenue p@(Paper{..}) = (i_ $ toHtml journal) <> ", " <> t
  where t = case (yrSpEp p) of
              Just (yr, sp, ep) -> vol <> num <> ":"
                                <> (sHtml sp) <> "-"
                                <> (sHtml ep) <> ", "
                                <> (sHtml yr) <> "."
                                where
                                  vol = maybe mempty sHtml volume
                                  num = maybe mempty (\n -> "(" <> sHtml n <> ")") number
              Nothing -> "forthcoming."
wpVenue c@(Chapter{..}) = "In " <> (i_ $ toHtml booktitle)
                                <> ", ed "
                                <> (toHtml $ mconcat (intersperse ", " $ editor))
                                <> ". " <> t
  where t = case (yrSpEp c) of
              Just (yr, sp, ep) -> "Pages "
                                <> (sHtml sp) <> "-"
                                <> (sHtml ep) <> ", "
                                <> (toHtml publisher) <> " "
                                <> (sHtml yr) <> "."
              Nothing -> (toHtml publisher) <> " forthcoming."
wpVenue Book{..} = (toHtml publisher) <> " "
                                      <> (maybe "forthcoming." (\y -> (sHtml y) <> ".") year)

authname :: Text -> Maybe Text
authname t = name <$> M.lookup t authors

getText :: Maybe Text -> Text
getText m =
  case m of
    Just t -> t
    Nothing -> mempty

bibTeXauths :: WritingPiece -> Text
bibTeXauths = btChars . T.intercalate " and " . map (getText . authname) . wpAuthorTags

btChars :: Text -> Text
btChars = T.concatMap cleanup
  where
    cleanup c =
      case c of
        '\225' -> "{\\'{a}}"
        '\233' -> "{\\'{e}}"
        '\237' -> "{\\'{i}}"
        '\252' -> "{\\\"{u}}"
        _      -> T.singleton c

write :: Show a => a -> Text
write = T.pack . show

wpBibtex :: WritingPiece -> Text
wpBibtex p@(Paper{..}) = T.concat $
  [ "@article{", bibtag
  , ",\n   author = {", bibTeXauths p
  , "},\n   title = {", title
  , "},\n   journal = {", journal
  , "},\n   "
  ] ++ rest ++
  [ "}\n" ]
  where 
    rest = case (yrSpEp p) of
             Just (yr, sp, ep) -> [ "year = {" , write yr
                                  , "},\n   volume = {", vol
                                  , "},\n   number = {", nmb
                                  , "},\n   pages = {"
                                  , (write sp) <> "--" <> (write ep)
                                  , "}\n"
                                  ]
             Nothing -> [ "note = {Forthcoming}\n" ]
    vol = maybe mempty write volume
    nmb = maybe mempty write number
wpBibtex c@(Chapter{..}) = T.concat $
  [ "@incollection{", bibtag
  , ",\n   author = {", bibTeXauths c
  , "},\n   title = {", title
  , "},\n   booktitle = {", booktitle
  , "},\n   editor = {"
  , btChars . T.intercalate " and " $ editor
  , "},\n   publisher = {", publisher
  , "},\n   "
  ] ++ rest ++
  [ "}\n" ]
  where 
    rest = case (yrSpEp c) of
             Just (yr, sp, ep) -> [ "year = {", write yr
                                  , "},\n   pages = {"
                                  , (write sp) <> "--" <> (write ep)
                                  , "}\n"
                                  ]
             Nothing -> [ "note = {Forthcoming}\n" ]
wpBibtex b@(Book{..}) = T.concat $
  [ "@book{", bibtag
  , ",\n   author = {", bibTeXauths b
  , "},\n   title = {", title
  , "},\n   publisher = {", publisher
  , "},\n   "
  ] ++ rest ++
  [ "}\n" ]
  where
    rest = case year of
             Just yr -> [ "year = {", write yr, "}\n" ]
             Nothing -> [ "note = {Forthcoming}\n" ]

wpFile :: FilePath
wpFile = "./src/writing.yaml"

pieces :: IO (Either Y.ParseException [WritingPiece])
pieces = do
  pData <- BS.readFile wpFile
  return (Y.decodeEither' pData)
