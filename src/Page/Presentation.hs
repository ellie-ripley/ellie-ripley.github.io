{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Presentation (presentationPage) where

import Data.Aeson.TH
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as Y
import qualified Data.ByteString as BS
import Lucid
import Lucid.Bootstrap

import Authors (Author(..), me, cervr, authorLookup, makeAuthorLink)
import PageTemplate (navbarJS, pageFrom, topLabel)
import WebsiteTools (AuthorCat(..), leftsRights, listItems)

-- SECTION: Prepare the types

data PresExtraType = Slides | Handout | Video | OtherExtra deriving (Eq)

type PELinkURL = Text
type PELinkWord = Text
data PresExtras = PE PresExtraType PELinkURL PELinkWord

data PresentationRaw = P { presTitle :: Text
                         , presAuthors :: AuthorCat
                         , presLocations :: [ Text ]
                         , presExtras :: [ PresExtras ]
                         }

newtype Presentation = Presentation (PresentationRaw, [Author])

deriveJSON defaultOptions ''PresExtraType
deriveJSON defaultOptions ''PresExtras
deriveJSON defaultOptions ''PresentationRaw

-- SECTION: Read the data

presFile :: FilePath
presFile = "./src/presentations.yaml"

getPresentationRaws :: IO (Either Y.ParseException [PresentationRaw])
getPresentationRaws = do
  pData <- BS.readFile presFile
  return (Y.decodeEither' pData)

presentationRaws :: IO [PresentationRaw]
presentationRaws = do
  ePres <- getPresentationRaws
  case ePres of
    Left pex -> do
      putStrLn $ "ERROR: Couldn't parse presentation file: " <> show presFile
      putStrLn $ show pex
      return []
    Right ps -> do
      putStrLn $ "Successfully parsed presentation file: " <> show presFile
      putStrLn $ "     " <> (show (length ps)) <> " records"
      return ps

elaboratePresentation :: [Author] -> PresentationRaw -> Either [Text] Presentation
elaboratePresentation auths pr =
  case presAuthors pr of
    Solo  -> Right $ Presentation (pr, [me])
    CERvR -> Right $ Presentation (pr, cervr)
    Other ts -> let (errs, as) = leftsRights $ map (flip authorLookup $ auths) ts
                in if null errs
                   then Right $ Presentation (pr, as)
                   else Left errs

elaboratePresentations :: [Author] -> [PresentationRaw] -> ([Text], [Presentation])
elaboratePresentations auths prs =
  let (errs, ps) = leftsRights $ map (elaboratePresentation auths) prs
  in (concat errs, ps)



-- SECTION: Put the page together

presentationPage :: [Author] -> IO (Html ())
presentationPage auths = do
  praws <- presentationRaws
  let (authErrs, pres) = elaboratePresentations auths praws
      bdy = presentationBody pres
  mapM_ (putStrLn . T.unpack) authErrs
  return $ pageFrom bdy (navbarJS "presentationlink")

presentationBody :: [Presentation] -> Html ()
presentationBody pres = bdy
  where ps = mconcat $ map presRow pres
        bdy = do topLabel "Presentations"
                 container_ $ do
                   div_ [class_ "mainbits"] ps

presRow :: Presentation ->  Html ()
presRow p@(Presentation (praw, _)) = rw
  where
    aths = presentationAuthors p
    rw = row_ [class_ "presentation-row"] $ do
              div_ [class_ "col-md-10 pres-bubble"] $ do
                   row_ [] $ do
                        div_ [class_ "col-md-5"]
                             ((p_ [class_ "talktitle"] (toHtml $ presTitle praw)) <> aths)
                        div_ [class_ "col-md-7"]
                             (ul_ [class_ "presentation-venue"]
                                  (listItems [class_ "presentation-venue"] (map toHtml $ presLocations praw)))
              extrasMarks p

extraMark :: PresExtraType -> Html ()
extraMark pe = span_ [class_ ("fa fa-fw " <> pec)] ""
  where pec = case pe of
                   Slides     -> "fa-desktop"
                   Handout    -> "fa-paper-plane" --file-text-o, paper-plane
                   Video      -> "fa-video-camera" --video-camera, film
                   OtherExtra -> "fa-chain"

extraRow :: PresExtras -> Html ()
extraRow (PE ty ur tx) =
    (tr_ $ do
       (td_ [class_ "contact-icon"]  (a_ [href_ ur, target_ "_blank"] (extraMark ty)))
       (td_ (a_ [href_ ur, target_ "_blank"] $ toHtml tx)))

extrasMarks :: Presentation -> Html ()
extrasMarks (Presentation (praw, _))
  | null pes = mempty
  | otherwise = div_ [class_ "col-md-2 extra-marks"]
                    (table_ $ (mconcat $ map extraRow pes))
  where pes = presExtras praw

presentationAuthors :: Presentation -> Html ()
presentationAuthors (Presentation (_, as)) =
  let aths = map makeAuthorLink as
  in p_ [class_ "presentation-authors" ] (mconcat $ intersperse ", " aths)
