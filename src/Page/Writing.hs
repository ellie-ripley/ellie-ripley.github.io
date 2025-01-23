{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Writing (writingPage) where

import Data.List (intersperse, sortBy)
import qualified Data.Text as T
import Lucid
import Lucid.Bootstrap
import Text.BibTeX.Entry (T(..))
import Text.BibTeX.Parse
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (parseFromFile)

import Authors (makeAuthorLink)
import PageTemplate (navbarJS, pageFrom, topLabel)
import WebsiteTools
import Writing ( WritingPieceRaw(..)
               , WritingPiece(..)
               , elaborateWPs
               , toWritingPieceRaw
               , wpVenue
               , wpBibtex
               )

-- SECTION: Load the data

wpFile :: FilePath
wpFile = "./src/writing.bib"

getWritingBibEntries :: IO (Either ParseError [T])
getWritingBibEntries = parseFromFile (skippingLeadingSpace file) wpFile

writingPieceRaws :: IO [WritingPieceRaw]
writingPieceRaws = do
  eBEs <- getWritingBibEntries
  case eBEs of
    Left pex -> do
      putStrLn $ "ERROR: Couldn't parse writing file: " <> show wpFile
      putStrLn $ show pex
      return []
    Right bes -> do
      putStrLn $ "Successfully parsed writing file: " <> show wpFile
      putStrLn $ "     " <> (show (length bes)) <> " records"
      let (errs, wprs) = leftsRights $ map toWritingPieceRaw bes
          msg = if null errs
                  then "     All writing entries understood."
                  else "     Failed to understand writing entries: " <>
                       mconcat (intersperse ", " errs)
      putStrLn msg
      return wprs

-- SECTION: Build the page

writingPage :: [Author] -> IO (Html ())
writingPage auths = do
  bes <- writingPieceRaws
  let (authErrs, wps) = elaborateWPs auths bes
      msg = if null authErrs
               then "All writing authors found."
               else ""
  putStrLn msg
  let bdy = writingBody wps
  mapM_ (putStrLn . T.unpack) authErrs
  return $ pageFrom bdy (navbarJS "writinglink" <> searchJS <> popoverJS)

searchJS :: Html ()
searchJS = script_ [src_ "./js/search.js"] ""

popoverJS :: Html ()
popoverJS = script_ [src_ "./js/popover.js"] ""


searchBar :: Html ()
searchBar = div_ [class_ "input-group"] $ do
  (span_ [class_ "input-group-text"] (span_ [class_ "fa fa-search"] ""))
  (input_ [class_ "form-control", id_ "title-search-box", type_ "text", placeholder_ "Title search"])

searchSort :: Html ()
searchSort = mempty

searchFilters :: Html ()
searchFilters = div_ $ do
  (h6_ [class_ "filterhead"] "Filter by author:")
  (form_ [action_ ""] $ do
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-solo"]) <> " Just Ellie")
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-cervr"]) <> " CERvR "
      <> (a_ [ id_ "cervr-info"
             , (term "tabindex") "0"
             , (term "data-bs-toggle") "popover"
             , (term "data-bs-trigger") "hover"
             , title_ "CERvR is:"
             , (term "data-bs-html") "true"
             , (term "data-bs-content") "Pablo Cobreros, <br> Paul Egré, <br> Ellie Ripley, <br> Robert van Rooij"
             ] "[?]"))
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-other"]) <> " Other combinations"))

alsoSeeBit :: Html ()
alsoSeeBit = p_ [class_ "also-see"]
                ("Also see my "
                    <> (lk "https://philpapers.org/profile/12303" "philpapers profile")
                    <> " or my "
                    <> (lk "https://orcid.org/0000-0002-3356-0771" "ORCID page")
                    <> ".")

writingBody :: [WritingPiece] -> Html ()
writingBody ps = bdy
    where pcs = mconcat $ map makeEntry (zip (sortBy pieceSort ps) [1..])
          bdy = do
                    topLabel "Writing"
                    container_ [class_ "mainbits"] $
                      row_ $ do
                        div_ [class_ "col-md-3 searchbar"]
                            (searchBar <> searchSort <> searchFilters <> alsoSeeBit)
                        div_ [class_ "col-md-9 searchresults"]
                            (ul_ [class_ "writingdisplay"] pcs)

paperTitleHead :: WritingPieceRaw -> Html ()
paperTitleHead p =
  case (writingUrl p) of
    "" -> pt
    u  -> a_ [ href_ u
             , class_ "title-link"
             , target_ "_blank"
             ] pt
  where pt = toHtml . filter (\c -> c /= '{' && c /= '}') . T.unpack $ title p


-- | returns pair of Html entry and list of errors to be printed at console
makeEntry :: (WritingPiece, Int) -> Html ()
makeEntry (p@(WritingPiece (wpr, as)), n) = ent
  where cls = "paperbubble " <> (classify $ internalAuthors wpr)
        auths = map makeAuthorLink as
        nt = T.pack $ show n
        ci = "citation" <> nt
        ai = "abstract" <> nt
        bi = "bibtex" <> nt
        atab = case abstract wpr of
                    Nothing -> mempty
                    Just _  -> button_ [ class_ "nav-link"
                                       , id_ (ai <> "-tab")
                                       , term "data-bs-toggle" "tab"
                                       , term "data-bs-target" ("#" <> ai)
                                       , type_ "button"
                                       , role_ "tab"
                                       , term "aria-controls" ai
                                       , term "aria-selected" "false"
                                       ] "Abstract"
        apane = case abstract wpr of
                    Nothing -> mempty
                    Just ab -> div_ [ role_ "tabpanel"
                                    , class_ "tab-pane"
                                    , id_ ai
                                    ]
                                    (p_ [class_ "abstract"] (toHtml ab))
        ent = li_ [class_ cls] $ do
                   div_ [class_ "row"] $ do
                     p_ [class_ "ptitle"] (paperTitleHead wpr)
                     p_ [class_ "pauthors"] (mconcat $ intersperse ", " auths)
                     div_ [class_ "col paperinfo"] $ do
                          nav_ [] (div_ [ class_ "nav nav-tabs"
                                        , id_ ("nav-tab-" <> nt)
                                        , role_ "tablist"
                                        ] $ do
                                          button_ [ class_ "nav-link active"
                                                  , id_ (ci <> "-tab")
                                                  , term "data-bs-toggle" "tab"
                                                  , term "data-bs-target" ("#" <> ci)
                                                  , type_ "button"
                                                  , role_ "tab"
                                                  , term "aria-controls" ci
                                                  , term "aria-selected" "true"
                                                  ] "Citation"
                                          atab
                                          button_ [ class_ "nav-link"
                                                  , id_ (bi <> "-tab")
                                                  , term "data-bs-toggle" "tab"
                                                  , term "data-bs-target" ("#" <> bi)
                                                  , type_ "button"
                                                  , role_ "tab"
                                                  , term "aria-controls" bi
                                                  , term "aria-selected" "false"
                                                  ] "BibTeX"
                                  )
                          div_ [class_ "tab-content"] $ do
                               div_ [ role_ "tabpanel"
                                    , class_ "tab-pane active"
                                    , id_ ci
                                    , term "aria-labelledby" (ci <> "-pill")
                                    ]
                                    (p_ [class_ "pvenue"] (wpVenue wpr))
                               apane
                               div_ [ role_ "tabpanel"
                                    , class_ "tab-pane"
                                    , id_ bi
                                    , term "aria-labelledby" (bi <> "-pill")
                                    ]
                                    (p_ [class_ "bibtex"] (pre_ [class_ "bibtex"] (toHtml $ wpBibtex p)))




pieceSort :: WritingPiece -> WritingPiece -> Ordering
pieceSort (WritingPiece (p1, _)) (WritingPiece (p2, _)) =
  case (year p1, year p2) of
    (Nothing, Nothing) -> nameSort
    (Nothing, _)       -> LT
    (_      , Nothing) -> GT
    (Just y1, Just z1) -> case z1 `compare` y1 of
                            EQ -> nameSort
                            x  -> x
  where
    nameSort = (title p1) `compare` (title p2)
