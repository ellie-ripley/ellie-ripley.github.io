{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Page.Index (indexPage) where

import Lucid
import Lucid.Bootstrap

import Links
import PageTemplate (navbarJS, pageFrom)

indexPage :: Html ()
indexPage = pageFrom indexBody (navbarJS "indexlink")

indexBody :: Html ()
indexBody =
  div_ [class_ "mainbits"] $
    container_ $
      row_ $ do
        div_ [class_ "col-md-6"] (img_ [class_ "img-rounded splashimg", src_ "./ellie-cropped.jpg"])
        div_ [class_ "col-md-6 mainbits"] indexBodyText

indexBodyText :: Html ()
indexBodyText =
     h1_ [class_ "good-morning"] "Good morning!"
  <> p_ ("I'm Ellie Ripley (they/she), a member of the "
        <> monashPhilLink "philosophy department"
        <> " at Monash University.")
  <> p_ "My research focuses on languages, logics, and the relations between them."
  <> p_ ("I'm also a member of the "
        <> aalLink "Australasian Association for Logic"
        <> " and the "
        <> lllLink "Logicians' Liberation League"
        <> ".")
  <> p_ ("You can email me at "
        <> emailLinkFull
        <> ".")
  <> p_ ("Citation: please cite my work under the name \"Ellie Ripley\","
         <> " regardless of the original name of publication.")
