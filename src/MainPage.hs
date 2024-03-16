{-# LANGUAGE OverloadedStrings #-}

module MainPage where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

heading :: Html
heading =
    do
      H.title "Игра 8885"
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
      link ! rel "stylesheet" ! type_ "text/css" ! href "css/pure-min.css"
--      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Victor Mono"
      link ! rel "stylesheet" ! type_ "text/css" ! href "css/styles.css"
      link ! rel "icon" ! type_ "image/png" ! sizes "32x32" ! href "/icon/favicon-32x32.png"
      link ! rel "icon" ! type_ "image/png" ! sizes "16x16" ! href "/icon/favicon-16x16.png"
      pageScript

pageScript :: Html
pageScript =
    do
      script ! src "js/apexcharts.min.js" ! A.type_ "text/javascript" $ ""
      script ! src "js/main.js" ! A.type_ "text/javascript" $ ""

pageContent :: Html
pageContent =
    do
      H.div ! A.id "chart" ! A.class_ "chart" $ return ()

mainPage :: Html
mainPage =
    docTypeHtml $ do
      H.head heading
      body $
           do
             H.div ! A.id "layout" $ pageContent

