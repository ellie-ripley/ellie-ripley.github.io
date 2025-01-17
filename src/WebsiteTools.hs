{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module WebsiteTools where

import Lucid
import Data.Text (Text)

import Data.Aeson
import Data.Aeson.TH

data AuthorCat = Solo | CERvR | Other [Text] deriving (Show, Eq)

deriveJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''AuthorCat

class Classify a where
  classify :: a -> Text

instance Classify AuthorCat where
  classify Solo = "solo"
  classify CERvR = "cervr"
  classify (Other _) = "other"

pileUp :: [Html ()] -> Html ()
pileUp = foldr (<>) mempty

pileUpPair :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
pileUpPair ab = (mconcat as, mconcat bs)
  where (as, bs) = unzip ab

listItems :: [Attribute] -> [Html ()] -> Html ()
listItems atts ts = pileUp (map listItem ts)
  where
    listItem t = li_ atts t

lk :: Text -> Html () -> Html ()
lk u t = a_ [href_ u, target_ "_blank"] t

doiToLink :: Text -> Html ()
doiToLink d = lk lnk "DOI link"
  where lnk = "http://dx.doi.org/" <> d

sHtml :: (Show a, Monad m) => a -> HtmlT m ()
sHtml = toHtml . show

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

leftsRights :: [Either a b] -> ([a], [b])
leftsRights [] = ([], [])
leftsRights (Left  x : zs) = mapFst (x:) (leftsRights zs)
leftsRights (Right y : zs) = mapSnd (y:) (leftsRights zs)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)
