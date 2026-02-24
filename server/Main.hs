-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BL8
-----------------------------------------------------------------------------
import           Miso
import           Miso.Router hiding (href_)
import           Miso.Html hiding (title_)
import qualified Miso.Html as H
import           Miso.Html.Property
-----------------------------------------------------------------------------
import           Types
-----------------------------------------------------------------------------
main :: IO ()
main = do
  BL8.writeFile ("public/" <> "index.html") $ toHtml (template Index)
  putStrLn "Wrote to public/index.html..."
  BL8.writeFile ("public/" <> "introduction.html") $ toHtml (template Introduction)
  putStrLn "Wrote to public/introduction.html..."
  BL8.writeFile ("public/" <> "install.html") $ toHtml (template Install)
  putStrLn "Wrote to public/install.html..."
  BL8.writeFile ("public/chapter/" <> "1.html") $ toHtml (template (Chapter (Capture 1)))
  putStrLn "Wrote to public/chapter/1.html..."
  BL8.writeFile ("public/chapter/" <> "2.html") $ toHtml (template (Chapter (Capture 2)))
  putStrLn "Wrote to public/chapter/2.html..."
  BL8.writeFile ("public/chapter/" <> "3.html") $ toHtml (template (Chapter (Capture 3)))
  putStrLn "Wrote to public/chapter/3.html..."
-----------------------------------------------------------------------------
template :: Route -> [View m a]
template r =
  [ doctype_
  , html_ [ lang_ "en", class_ "dark theme-claude" ]
    [ head_
      []
      [ meta_ [charset_ "utf-8"]
      , meta_
          [ content_ "width=device-width, initial-scale=1"
          , name_ "viewport"
          ]
      , H.title_ [] ["miso-book"]
      , H.script_
          [ src_ "index.js", type_ "module"
          ] mempty
      , H.script_
          [ src_ "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.11/dist/js/all.min.js"
          ] mempty
      , link_
          [rel_ "stylesheet", href_ "/assets/styles.css"]
      , meta_
          [ content_ "en"
          , textProp "http-equiv" "Content-Language"
          ]
      , meta_
          [ content_
              "A miso component library built on Tailwind, ShadCN and Basecoat CSS"
          , name_ "description"
          ]
      , meta_
          [ content_
              "components,component library,component system,miso, haskell, haskell-miso, UI,UI kit,shadcn,shadcn/ui,Tailwind CSS,Tailwind,CSS,HTML,monads,JS,JavaScript,vanilla JS,vanilla JavaScript"
          , name_ "keywords"
          ]
      , link_
          [ href_ "/assets/favicon.ico"
          , type_ "image/x-icon"
          , rel_ "icon"
          ]
      , link_
          [ href_ "/assets/apple-touch-icon.png"
          , textProp "sizes" "180x180"
          , rel_ "apple-touch-icon"
          ]
      , meta_ [content_ "website", textProp "property" "og:type"]
      , meta_
          [ content_ "https://book.haskell-miso.org"
          , textProp "property" "og:url"
          ]
      , meta_
          [ content_ "Haskell miso | miso-book "
          , textProp "property" "og:title"
          ]
      , meta_
          [ content_
              "A miso component library built on Tailwind, ShadCN and Basecoat CSS"
          , textProp "property" "og:description"
          ]
      , meta_
          [ content_
              "https://book.haskell-miso.org/assets/social-screenshot.png"
          , textProp "property" "og:image"
          ]
      , meta_
          [content_ "miso-book", textProp "property" "og:sitename"]
      , meta_ [content_ "en_US", textProp "property" "og:locale"]
      , meta_ [content_ "dmjio", textProp "property" "og:author"]
      , meta_
          [ content_ "summary_large_image"
          , name_ "twitter:card"
          ]
      , meta_
          [ content_ "https://book.haskell-miso.org"
          , name_ "twitter:url"
          ]
      , meta_
          [content_ "miso-book", name_ "twitter:title"]
      , meta_
          [ content_
              "A collection of all the components available in miso-book"
          , name_ "twitter:description"
          ]
      , meta_
          [ content_
              "https://book.haskell-miso.org/assets/social-screenshot.png"
          , name_ "twitter:image"
          ]
      , meta_
          [content_ "@dmjio", name_ "twitter:creator"]
      , script_
          [ src_
              "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"
          ]
          mempty
      , link_
          [ href_
              "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/dark.min.css"
          , rel_ "stylesheet"
          ]
      , script_
          [ src_
              "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/haskell.min.js"
          ]
          mempty
      ]
    , script_ []
      """
      (() => {
           try {
             const stored = localStorage.getItem('themeMode');
             if (stored ? stored === 'dark'
                        : matchMedia('(prefers-color-scheme: dark)').matches) {
               document.documentElement.classList.add('dark');
             }
           } catch (_) {}
           const apply = dark => {
             document.documentElement.classList.toggle('dark', dark);
             try { localStorage.setItem('themeMode', dark ? 'dark' : 'light'); } catch (_) {}
           };
           document.addEventListener('basecoat:theme', (event) => {
             const mode = event.detail?.mode;
             apply(mode === 'dark' ? true
                  : mode === 'light' ? false
                  : !document.documentElement.classList.contains('dark'));
           });
      })();
      """
    , style_ [] """
        body {
          overflow-x: hidden;
          max-width: 100%;
        }
      """
     , body_ [] [ mount_ (app (toURI r)) ]
   ]
 ]
-----------------------------------------------------------------------------
