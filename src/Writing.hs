{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Writing where

import Data.Char (toLower)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Lucid ( i_, Html, ToHtml(toHtml) )
import Data.List (intersperse)


import Text.BibTeX.Entry ( T(..) )
import Text.Read (readMaybe)

import Authors (Author(..), me, cervr, authorLookup)
import WebsiteTools (AuthorCat(..), sHtml, leftsRights, splitAtCommasRemoveSpaces)

data WritingPieceRaw =
      Paper { title :: Text
            , internalAuthors :: AuthorCat
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
              , internalAuthors :: AuthorCat
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
           , internalAuthors :: AuthorCat
           , writingUrl :: Text
           , bibtag :: Text
           , abstract :: Maybe Text
           , year :: Maybe Int
           , publisher :: Text
           }
   deriving (Show, Eq)

newtype WritingPiece = WritingPiece (WritingPieceRaw, [Author])




-- Converting BibTeX to WritingPieceRaw

dropLeadingHyphens :: String -> String
dropLeadingHyphens ('-':rest) = dropLeadingHyphens rest
dropLeadingHyphens str = str

maybePages :: String -> (Maybe Int, Maybe Int)
maybePages str =
  let (msp, s2) = break (== '-') str
      mep = dropLeadingHyphens s2
  in  (readMaybe msp, readMaybe mep)

readAcat :: String -> AuthorCat
readAcat s
  | (map toLower s) == "me" = Solo
  | (map toLower s) == "solo" = Solo
  | (map toLower s) == "cervr" = CERvR
  | otherwise = Other $ map pack (splitAtCommasRemoveSpaces s)


maybeArticle :: T -> Maybe WritingPieceRaw
maybeArticle t
  | entryType t /= "article" = Nothing
  | otherwise = do
      let look k = lookup k (fields t)
      mpgs <- look "pages"
      let (msp, mep) = maybePages mpgs
      ttl <- look "title"
      jnl <- look "journal"
      wurl <- look "writingUrl"
      acat <- look "acat"
      return $ Paper { title = pack ttl
                     , internalAuthors = readAcat acat
                     , writingUrl = pack wurl
                     , bibtag = pack $ identifier t
                     , abstract = pack <$> look "abstract"
                     , year = look "year" >>= readMaybe
                     , startPage = msp
                     , endPage = mep
                     , journal = pack jnl
                     , volume = look "volume" >>= readMaybe
                     , number = look "number" >>= readMaybe
                     , doi = pack <$> look "doi"
                     }

maybeBook :: T -> Maybe WritingPieceRaw
maybeBook t
  | entryType t /= "book" = Nothing
  | otherwise = do
      let look = \k -> lookup k (fields t)
      ttl <- look "title"
      wurl <- look "writingUrl"
      pub <- look "publisher"
      acat <- look "acat"
      return $ Book { title = pack ttl
                    , internalAuthors = readAcat acat
                    , writingUrl = pack wurl
                    , bibtag = pack $ identifier t
                    , abstract = pack <$> look "abstract"
                    , year = look "year" >>= readMaybe
                    , publisher = pack pub
                    }

maybeChapter :: T -> Maybe WritingPieceRaw
maybeChapter t
  | entryType t /= "incollection" = Nothing
  | otherwise = do
      let look = \k -> lookup k (fields t)
      mpgs <- look "pages"
      let (msp, mep) = maybePages mpgs
      ttl <- look "title"
      wurl <- look "writingUrl"
      bkt <- look "booktitle"
      edstring <- look "editor"
      pub <- look "publisher"
      acat <- look "acat"
      return $ Chapter { title = pack ttl
                       , internalAuthors = readAcat acat
                       , writingUrl = pack wurl
                       , bibtag = pack $ identifier t
                       , abstract = pack <$> look "abstract"
                       , year = look "year" >>= readMaybe
                       , startPage = msp
                       , endPage = mep
                       , booktitle = pack bkt
                       , editor = [pack edstring]
                       , publisher = pack pub
                       }

elaborateWP :: [Author] -> WritingPieceRaw -> Either Text WritingPiece
elaborateWP auths wpr =
  case internalAuthors wpr of
    Solo  -> Right $ WritingPiece (wpr, [me])
    CERvR -> Right $ WritingPiece (wpr, cervr)
    Other ts -> let (errs, as) = leftsRights $ map (flip authorLookup $ auths) ts
                in if null errs
                   then Right $ WritingPiece (wpr, as)
                   else Left $ "Couldn't find authors in writing piece "
                               <> title wpr <> ":\n     "
                               <> T.intercalate "\n     " errs <> "\n"

elaborateWPs :: [Author] -> [WritingPieceRaw] -> ([Text], [WritingPiece])
elaborateWPs auths wprs = leftsRights $ map (elaborateWP auths) wprs


toWritingPieceRaw :: T -> Either String WritingPieceRaw
toWritingPieceRaw t
  | Just a <- maybeArticle t = Right a
  | Just b <- maybeBook t    = Right b
  | Just c <- maybeChapter t = Right c
  | otherwise = Left (identifier t)

--Accessors:

yrSpEp :: WritingPieceRaw -> Maybe (Int, Int, Int)
yrSpEp (Paper{..}) = case (catMaybes [year, startPage, endPage]) of
  [yr, sp, ep] -> Just (yr, sp, ep)
  _            -> Nothing
yrSpEp (Chapter{..}) = case (catMaybes [year, startPage, endPage]) of
  [yr, sp, ep] -> Just (yr, sp, ep)
  _            -> Nothing
yrSpEp (Book{}) = Nothing

wpAuthorTags :: WritingPieceRaw -> [Text]
wpAuthorTags p = case internalAuthors p of
                      Solo     -> ["me"]
                      CERvR    -> [ "pabloCobreros"
                                  , "paulEgre"
                                  , "me"
                                  , "robertVanRooij"
                                  ]
                      Other as -> as

wpVenue :: WritingPieceRaw -> Html ()
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



bibTeXauths :: [Author] -> Text
bibTeXauths = btChars . T.intercalate " and " . map name

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
wpBibtex (WritingPiece (wpr, as)) =
  case wpr of
    p@(Paper{..}) -> T.concat $
        [ "@article{", bibtag
        , ",\n   author = {", bibTeXauths as
        , "},\n   title = {", title
        , "},\n   journal = {", journal
        , "},\n   writingUrl = {", writingUrl
        , "},\n   abstract = {", abst
        , "},\n   "
        ] ++ rest ++ di ++
        [ "}\n" ]
        where
          rest = case (yrSpEp p) of
                  Just (yr, sp, ep) -> [ "year = {" , write yr
                                        , "},\n   volume = {", vol
                                        , "},\n   number = {", nmb
                                        , "},\n   pages = {"
                                        , (write sp) <> "--" <> (write ep)
                                        , "},\n   "
                                        ]
                  Nothing -> [ "note = {Forthcoming}\n" ]
          di = case doi of
                Nothing -> []
                Just d -> ["doi = {", d, "},\n"]
          vol = maybe mempty write volume
          nmb = maybe mempty write number
          abst = maybe mempty id abstract
    c@(Chapter{..}) -> T.concat $
        [ "@incollection{", bibtag
        , ",\n   author = {", bibTeXauths as
        , "},\n   title = {", title
        , "},\n   booktitle = {", booktitle
        , "},\n   editor = {"
        , btChars . T.intercalate " and " $ editor
        , "},\n   publisher = {", publisher
        , "},\n   abstract = {", abst
        , "},\n   writingUrl = {", writingUrl
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
          abst = maybe mempty id abstract
    (Book{..}) -> T.concat $
        [ "@book{", bibtag
        , ",\n   author = {", bibTeXauths as
        , "},\n   title = {", title
        , "},\n   publisher = {", publisher
        , "},\n   abstract = {", abst
        , "},\n   writingUrl = {", writingUrl
        , "},\n   "
        ] ++ rest ++
        [ "}\n" ]
        where
          rest = case year of
                  Just yr -> [ "year = {", write yr, "}\n" ]
                  Nothing -> [ "note = {Forthcoming}\n" ]
          abst = maybe mempty id abstract
