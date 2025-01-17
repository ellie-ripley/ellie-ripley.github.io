{-# LANGUAGE OverloadedStrings #-}

module Links where

import Data.Text (Text)
import Lucid

import WebsiteTools (lk)

aalLink :: Html () -> Html ()
aalLink = lk "http://aalogic.org"

aalLinkFull :: Html ()
aalLinkFull = aalLink "AAL"

aapLink :: Html () -> Html ()
aapLink = lk "http://www.aap-conferences.org.au/"

aapLinkFull :: Html ()
aapLinkFull = aapLink "AAP"

ajlLink :: Html () -> Html ()
ajlLink = lk "http://ojs.victoria.ac.nz/ajl"

aslLink :: Html () -> Html ()
aslLink = lk "http://www.aslonline.org"

myEmailAddress :: Text
myEmailAddress = "ripley@negation.rocks"

emailLink :: Html () -> Html ()
emailLink = lk $ "mailto:" <> myEmailAddress

emailLinkFull :: Html ()
emailLinkFull = emailLink $ toHtml myEmailAddress

ijnLink :: Html () -> Html ()
ijnLink = lk "http://www.institutnicod.org"

ijnLinkFull :: Html ()
ijnLinkFull = ijnLink "Insitut Jean Nicod"

lllLink :: Html () -> Html ()
lllLink = lk "https://sites.google.com/view/aalogic/curios/logicians-liberation-league"

mcmpLink :: Html () -> Html ()
mcmpLink = lk "http://www.mcmp.philosophie.uni-muenchen.de/index.html"

mcmpLinkFull :: Html ()
mcmpLinkFull = mcmpLink "MCMP"

monashLink :: Html () -> Html ()
monashLink = lk "http://www.monash.edu.au"

monashPhilLink :: Html () -> Html ()
monashPhilLink = id

monashSchoolLink :: Html () -> Html ()
monashSchoolLink = id

mlfcLink :: Html () -> Html ()
mlfcLink = lk "https://www.monash.edu/it/mlfc"

mlfcLinkFull :: Html ()
mlfcLinkFull = mlfcLink "MLFC"

openAccessLink :: Html () -> Html ()
openAccessLink = lk "http://www.jasonmkelly.com/2013/01/27/green-gold-and-diamond-a-short-primer-on-open-access"

rslLink :: Html () -> Html ()
rslLink = lk "http://journals.cambridge.org/RSL"

uconnLink :: Html () -> Html ()
uconnLink = lk "http://www.uconn.edu"

uconnLogicLink :: Html () -> Html ()
uconnLogicLink = lk "http://logic.uconn.edu"

uconnPhilLink :: Html () -> Html ()
uconnPhilLink = lk "http://philosophy.uconn.edu"
