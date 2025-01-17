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

import Authors (Author(..), eAuthors, makeAuthorLink)
import PageTemplate (navbarJS, pageFrom, topLabel)
import WebsiteTools (AuthorCat(..), leftsRights, listItems, pileUpPair)

-- SECTION: Prepare the types

data PresExtraType = Slides | Handout | Video | OtherExtra deriving (Eq)

type PELinkURL = Text
type PELinkWord = Text
data PresExtras = PE PresExtraType PELinkURL PELinkWord

data Presentation = P { presTitle :: Text
                      , presAuthors :: AuthorCat
                      , presLocations :: [ Text ]
                      , presExtras :: [ PresExtras ]
                      }

deriveJSON defaultOptions ''PresExtraType
deriveJSON defaultOptions ''PresExtras
deriveJSON defaultOptions ''Presentation

-- SECTION: Read the data

presentations :: IO (Either Y.ParseException [Presentation])
presentations = do
  pData <- BS.readFile presFile
  return (Y.decodeEither' pData)

presFile :: FilePath
presFile = "./src/presentations.yaml"

-- SECTION: Put the page together

presentationPage :: IO (Html ())
presentationPage = do
  ePres <- presentations
  eAuths <- eAuthors
  let pres = either (const []) id ePres
      auths = either (const []) id eAuths
      (bdyErrs, bdy) = presentationBody auths pres
  mapM_ (putStrLn . T.unpack) bdyErrs
  return $ pageFrom bdy (navbarJS "presentationlink")

presentationBody :: [Author] -> [Presentation] -> ([Text], Html ())
presentationBody auths pres = (errs, bdy)
  where (errs, ps) = pileUpPair $ map (presRow auths) pres
        bdy = do topLabel "Presentations"
                 container_ $ do
                   div_ [class_ "mainbits"] ps

presRow :: [Author] -> Presentation -> ([Text], Html ())
presRow auths p = (ers, rw)
  where
    (ers, aths) = presentationAuthors auths (presAuthors p)
    rw = row_ [class_ "presentation-row"] $ do
              div_ [class_ "col-md-10 pres-bubble"] $ do
                   row_ [] $ do
                        div_ [class_ "col-md-5"]
                             ((p_ [class_ "talktitle"] (toHtml $ presTitle p)) <> aths)
                        div_ [class_ "col-md-7"]
                             (ul_ [class_ "presentation-venue"]
                                  (listItems [class_ "presentation-venue"] (map toHtml $ presLocations p)))
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
extrasMarks p
  | null pes = mempty
  | otherwise = div_ [class_ "col-md-2 extra-marks"]
                    (table_ $ (mconcat $ map extraRow pes))
  where pes = presExtras p


-- | [Text] in result is list of errors for console display
presentationAuthors :: [Author] -> AuthorCat -> ([Text], Html ())
presentationAuthors _ Solo = ([], mempty)
presentationAuthors auths CERvR =
  presentationAuthors auths(Other [ "pabloCobreros", "paulEgre", "me", "robertVanRooij" ])
presentationAuthors auths (Other as) =
  let (aErrs, aths) = leftsRights $ map (makeAuthorLink auths) as
  in (aErrs, p_ [class_ "presentation-authors" ] (mconcat $ intersperse ", " aths))
