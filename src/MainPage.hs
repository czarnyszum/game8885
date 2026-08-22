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
      link ! rel "stylesheet" ! type_ "text/css" ! href "css/styles.css"
      link ! rel "icon" ! type_ "image/png" ! sizes "32x32" ! href "/icon/favicon-32x32.png"
      link ! rel "icon" ! type_ "image/png" ! sizes "16x16" ! href "/icon/favicon-16x16.png"
      pageScript

pageScript :: Html
pageScript =
    do
      script ! src "js/apexcharts.min.js" ! A.type_ "text/javascript" $ ""
      script ! src "js/main.js" ! A.type_ "text/javascript" $ ""

controlsPanel :: Html
controlsPanel =
    H.div ! A.id "controls" ! A.class_ "controls" $ do
      H.label ! A.for "rule-select" $ "Правила:"
      H.select ! A.id "rule-select" $ return ()
      H.button ! A.id "btn-start" ! A.class_ "pure-button button-ctrl" $ "Старт"
      H.button ! A.id "btn-step"  ! A.class_ "pure-button button-ctrl" $ "Шаг"
      H.button ! A.id "btn-restart" ! A.class_ "pure-button button-ctrl" $ "Рестарт"

status :: Html
status = H.div ! A.id "status" ! A.class_ "status" $ return ()

pageContent :: Html
pageContent =
    H.div ! A.id "layout" $ do
      H.h1 ! A.class_ "title" $ "Игра 8885"
      controlsPanel
      status
      H.div ! A.id "chart" ! A.class_ "chart" $ return ()
      H.div ! A.id "hist" ! A.class_ "chart hist-chart" $ return ()

mainPage :: Html
mainPage =
    docTypeHtml $ do
      H.head heading
      body $ do
        pageContent
