{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Page.Index (indexPage) where

import Data.Text (Text)
import Lucid
import Lucid.Bootstrap

import Links
import PageTemplate (navbarJS, pageFrom, topLabel)

indexPage :: Html ()
indexPage = pageFrom indexBody (navbarJS "indexlink")

indexBody :: Html ()
indexBody =
  div_ [class_ "mainbits"] $
    container_ $
      row_ $ do
        div_ [class_ "col-md-6"] (img_ [class_ "img-rounded splashimg", src_ "./rockandroll.jpg"])
        div_ [class_ "col-md-6 mainbits"] indexBodyText

indexBodyText :: Html ()
indexBodyText =
     h1_ [class_ "good-morning"] "Good morning!"
  <> p_ ("I'm Ellie Ripley, a member of the "
        <> monashPhilLink "philosophy department"
        <> " at Monash University.")
  <> p_ "My research focuses on languages, logics, and the relations between them."
  <> p_ ("I'm also a member of the "
        <> aalLink "Australasian Association for Logic"
        <> " and the "
        <> mlfcLink "Monash Laboratory for the Foundations of Computing"
        <> ".")
  <> p_ ("You can email me at "
        <> emailLink "davewripley@gmail.com"
        <> ".")
