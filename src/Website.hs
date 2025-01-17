{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Website (websiteMain) where

import Data.Text.Lazy.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import Lucid

import PageTemplate (navbarJS, pageFrom, topLabel)

import Page.Index (indexPage)
import Page.Presentation (presentationPage)
import Page.Writing (writingPage)


--SECTION: exercises page

exercisePage :: Html ()
exercisePage = pageFrom exerciseBody (navbarJS "emulink")

exerciseBody :: Html ()
exerciseBody = do
  topLabel "EMU: Exercises for Model Understandingness"
  div_ [id_ "emu-app"] $ do
      script_ [src_ "./js/emu.js"] ""
      script_ [] "var app = Elm.Main.init({node: document.getElementById(\"emu-app\")});"
    


--SECTION: generation

dirPrefix :: FilePath
dirPrefix = "./docs/"


websiteMain :: IO ()
websiteMain = do
  System.Directory.createDirectoryIfMissing True dirPrefix
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "index.html") (renderText indexPage)
  pPage <- presentationPage
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "presentations.html") (renderText pPage)
  wPage <- writingPage
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "writing.html") (renderText wPage)
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "emu.html") (renderText exercisePage)
