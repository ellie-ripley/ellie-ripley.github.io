{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module PageTemplate where

import Data.Text (Text)
import Lucid
import Lucid.Bootstrap

import Links


topLabel :: Html () -> Html ()
topLabel lab = container_ (h1_ [class_ "toplabel"] lab)

scriptImports :: Html ()
scriptImports = do
  script_ [src_ "./js/jquery-2.1.3.min.js"] ""

pageFrom :: Html () -> Html() -> Html ()
pageFrom bod scrip = doctypehtml_
    (head_ htmlHeadBits) <> (body_ (pageHeader <> bod <> pageFooter <> scriptImports <> scrip))

navbarJS :: Text -> Html ()
navbarJS t = script_ [type_ "text/javascript"]
  ("var setActive = function () {\n$(\"." <> t <> "\").addClass(\"active\");\n};\n$(document).ready(setActive);")

htmlHeadBits :: Html ()
htmlHeadBits = meta_ [charset_ "utf-8"]
               <> meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
               <> meta_ [name_ "viewport"
                        , content_ "width=device-width, intial-scale=1"]
               <> meta_ [name_ "description", content_ "Ellie Ripley's website"]
               <> meta_ [name_ "author", content_ "Ellie Ripley"]
               <> title_ "Ellie Ripley"
               <> link_ [ href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/css/bootstrap.min.css"
                        , rel_ "stylesheet"
                        , integrity_ "sha384-BmbxuPwQa2lc/FVzBcNJ7UAyJxM6wuqIj61tLrc4wSX0szH/Ev+nYRRuWlolflfl"
                        , crossorigin_ "anonymous"
                        ]
               <> script_ [ src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/js/bootstrap.bundle.min.js"
                          , integrity_ "sha384-b5kHyXgcpbZJO/tY9Ul7kGkf1S0CWuKcCD38l8YkeH8z8QjE0GmW1gYU5S9FOnJ0"
                          , crossorigin_ "anonymous"
                          ] ""
               <> link_ [rel_ "stylesheet"
                        , href_ "./font-awesome-4.3.0/css/font-awesome.min.css"]
               <> link_ [rel_ "stylesheet", href_ "./css/ripley.css"]
               <> link_ [rel_ "apple-touch-icon"
                        , sizes_ "180x180"
                        , href_ "./apple-touch-icon.png"]
               <> link_ [rel_ "icon"
                        , type_ "image/png"
                        , sizes_ "32x32"
                        , href_ "./favicon-32x32.png"]
               <> link_ [rel_ "icon"
                        , type_ "image/png"
                        , sizes_ "16x16"
                        , href_ "./favicon-16x16.png"]
               <> link_ [rel_ "manifest"
                        , href_ "./site.webmanifest"]

pageHeader :: Html ()
pageHeader =
  nav_ [class_ "navbar navbar-expand-sm navbar-dark"] $
    div_ [class_ "container-fluid"] $ do
      a_ [ class_ "navbar-brand indexlink"
           , id_ "indexlink-id"
           , href_ "./index.html"
           ] "Ellie Ripley"
      button_ [ class_ "navbar-toggler"
              , type_ "button"
              , term "data-bs-toggle" "collapse"
              , term "data-bs-target" "#dr-headmenu"
              , term "aria-controls" "dr-headmenu"
              , term "aria-expanded" "false"
              , term "aria-lable" "Toggle navigation"
              ] (span_ [class_ "navbar-toggler-icon"] "")
      div_ [ id_ "dr-headmenu"
           , class_ "collapse navbar-collapse"
           ]
           (ul_ [class_ "navbar-nav me-auto mb-2 mb-sm-0"]
                   ((li_ [class_ "writinglink nav-item"]
                         (a_ [ class_ "nav-link"
                             , href_ "./writing.html"
                             ] "Writing"))
                 <> (li_ [class_ "presentationlink nav-item"]
                         (a_ [ class_ "nav-link"
                             , href_ "./presentations.html"
                             ] "Presentations"))
                 <> (li_ [class_ "emulink nav-item"]
                         (a_ [ class_ "nav-link"
                             , href_ "./emu.html"
                             ] "EMU"))
                 <> (li_ [class_ "cvlink nav-item"]
                         (a_ [ class_ "nav-link"
                             , href_ "./ripleyCV.pdf"
                             , target_ "_blank"
                             ] "CV"))))

pageFooter :: Html ()
pageFooter =
    container_ $
      row_ $ do
        div_ [class_ "col-md-2"] mempty
        div_ [class_ "col-md-8 footer"] $ do
          div_ [class_ "col-md-6"]
            (table_ $ do
              (tr_ $ do
                (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-inbox"] ""))
                (td_ (emailLinkFull))))
          div_ [class_ "snail col-md-6"]
            (table_ $ do
              (tr_ $ do
                (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-envelope"] ""))
                (td_ (monashPhilLink "Philosophy Department")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (monashSchoolLink "SOPHIS")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Building 11")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Monash University, VIC 3800")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Australia"))))
        div_ [class_ "col-md-2"] mempty
