{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Presentations (Presentation(..), extrasMarks, presentations) where

import Data.Text (Text)
import Data.Monoid (mempty, (<>))
import Lucid

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Yaml.Aeson as Y
import qualified Data.ByteString as BS

import WebsiteTools (AuthorCat(..), listItems, lk, pileUp)
import Links

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
                    (table_ $ (pileUp $ map extraRow pes))
  where pes = presExtras p

presFile :: FilePath
presFile = "./src/presentations.yaml"

presentations :: IO (Either Y.ParseException [Presentation])
presentations = do
  pData <- BS.readFile presFile
  return (Y.decodeEither' pData)
