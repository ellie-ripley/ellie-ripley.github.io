{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Writing (writingPage) where

import Data.List (intercalate, intersperse, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Bootstrap
import Text.BibTeX.Entry (T(..))
import Text.BibTeX.Parse
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (parseFromFile)

import Authors (Author(..), authorFile, eAuthors, makeAuthorLink)
import PageTemplate (navbarJS, pageFrom, topLabel)
import WebsiteTools (classify, leftsRights, lk, pileUpPair)
import Writing (WritingPiece(..), toWritingPiece, wpAuthorTags, wpVenue, wpBibtex)

-- SECTION: Load the data

wpFile :: FilePath
wpFile = "./src/writing.bib"

eitherWritingBibEntries :: IO (Either ParseError [T])
eitherWritingBibEntries = parseFromFile (skippingLeadingSpace file) wpFile


-- SECTION: Build the page

writingPage :: IO (Html ())
writingPage = do
  ewbe <- eitherWritingBibEntries
  case ewbe of
    Left perr -> do
      putStrLn $ "Couldn't read file " <> wpFile <> ": " <> show perr
      return mempty
    Right wps -> do
      let (bibErrs, ps) = leftsRights (map toWritingPiece wps)
      if null bibErrs
        then putStrLn "All bibtex entries read without error."
        else putStrLn $ "Couldn't read bibtex entries: " <> intercalate ", " bibErrs
      eAuths <- eAuthors
      case eAuths of
        Left aerr -> do
          putStrLn $ "Couldn't read file " <> authorFile <> ": " <> show aerr
          return mempty
        Right auths -> do
          putStrLn $ "Successfully read authors file."
          let (bdyErrs, bdy) = writingBody auths ps
          mapM_ (putStrLn . T.unpack) bdyErrs
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

-- | [Text] in return type is errors to be printed at console
writingBody :: [Author] -> [WritingPiece] -> ([Text], Html ())
writingBody as ps = (errs, bdy)
    where (errs, pcs) = pileUpPair $ map (makeEntry as) (zip (sortBy pieceSort ps) [1..])
          bdy = do
                    topLabel "Writing"
                    container_ [class_ "mainbits"] $
                      row_ $ do
                        div_ [class_ "col-md-3 searchbar"]
                            (searchBar <> searchSort <> searchFilters <> alsoSeeBit)
                        div_ [class_ "col-md-9 searchresults"]
                            (ul_ [class_ "writingdisplay"] pcs)

paperTitleHead :: WritingPiece -> Html ()
paperTitleHead p =
  case (writingUrl p) of
    "" -> pt
    u  -> a_ [ href_ u
             , class_ "title-link"
             , target_ "_blank"
             ] pt
  where pt = toHtml . filter (\c -> c /= '{' && c /= '}') . T.unpack $ title p


-- | returns pair of Html entry and list of errors to be printed at console
makeEntry :: [Author] -> (WritingPiece, Int) -> ([Text], Html ())
makeEntry as (p, n) = (errs, ent)
  where cls = "paperbubble " <> (classify $ authorCat p)
        (errs, auths) = leftsRights $ map (makeAuthorLink as) (wpAuthorTags p)
        nt = T.pack $ show n
        ci = "citation" <> nt
        ai = "abstract" <> nt
        bi = "bibtex" <> nt
        atab = case abstract p of
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
        apane = case abstract p of
                    Nothing -> mempty
                    Just ab -> div_ [ role_ "tabpanel"
                                    , class_ "tab-pane"
                                    , id_ ai
                                    ]
                                    (p_ [class_ "abstract"] (toHtml ab))
        ent = li_ [class_ cls] $ do
                   div_ [class_ "row"] $ do
                     p_ [class_ "ptitle"] (paperTitleHead p)
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
                                    (p_ [class_ "pvenue"] (wpVenue p))
                               apane
                               div_ [ role_ "tabpanel"
                                    , class_ "tab-pane"
                                    , id_ bi
                                    , term "aria-labelledby" (bi <> "-pill")
                                    ]
                                    (p_ [class_ "bibtex"] (pre_ [class_ "bibtex"] (toHtml $ wpBibtex as p)))




pieceSort :: WritingPiece -> WritingPiece -> Ordering
pieceSort p1 p2 =
  case (year p1, year p2) of
    (Nothing, Nothing) -> nameSort
    (Nothing, _)       -> LT
    (_      , Nothing) -> GT
    (Just y1, Just z1) -> case z1 `compare` y1 of
                            EQ -> nameSort
                            x  -> x
  where
    nameSort = (title p1) `compare` (title p2)
