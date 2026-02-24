-----------------------------------------------------------------------------
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE CPP                #-}
-----------------------------------------------------------------------------
module Types where
-----------------------------------------------------------------------------
import           GHC.Generics
-----------------------------------------------------------------------------
import           Miso hiding (view)
import           Miso.FFI.QQ (js)
import           Miso.Html hiding (data_)
import qualified Miso.Html.Property as P
import           Miso.Html.Property hiding (title_, label_, href_, form_)
import           Miso.Svg.Element hiding (title_)
import qualified Miso.Svg.Element as S
import           Miso.Svg.Property hiding (target_, path_)
import           Miso.Router
import           Miso.Lens hiding (view)
-----------------------------------------------------------------------------
data Action
  = ToggleDarkMode
  | ToggleSidebar
  | Highlight DOMRef
  | CopyButton DOMRef
  | ChangeRoute (Either RoutingError Route)
  | GoTo Route
-----------------------------------------------------------------------------
data Route
 = Index
 | Introduction
 | Install
 | Chapter (Capture "chapter" Int)
 deriving stock (Generic, Show)
 deriving anyclass Router
-----------------------------------------------------------------------------
app :: URI -> Component parent URI Action
app uri = (component uri update_ view)
  { subs = [ routerSub ChangeRoute ]
  , logLevel = DebugHydrate
  } where
      update_ = \case
        CopyButton domRef ->
          io_ [js| return copyButton(${domRef}); |]
        ToggleSidebar ->
          io_ [js| document.dispatchEvent (new CustomEvent('basecoat:sidebar')); |]
        ToggleDarkMode ->
          io_ [js| return document.dispatchEvent (new CustomEvent('basecoat:theme')); |]
        Highlight domRef ->
          io_ [js| return hljs.highlightElement(${domRef}); |]
        ChangeRoute (Left _) -> do
          io_ (consoleLog "couldn't route!")
        ChangeRoute (Right r) -> do
          this .= toURI r
        GoTo r -> do
          let u = toURI r
          io_ $ do
            consoleLog (prettyURI u)
            print u
          io_ (pushURI u)
          this .= u
-----------------------------------------------------------------------------
view :: URI -> View m Action
view uri = div_
    []
    [ aside_
        [ aria_ "hidden" "false"
        , data_ "side" "left"
        , class_ "sidebar "
        , id_ "sidebar"
        ]
        [ nav_
            [aria_ "label" "Sidebar navigation"]
            [ header_
                []
                [ a_
                    [ class_ "btn-ghost p-2 h-12 w-full justify-start"
                    , onClick (GoTo Index)
                    ]
                    [ div_
                        [ class_
                            "bg-sidebar-primary text-sidebar-primary-foreground flex aspect-square size-8 items-center justify-center rounded-lg"
                        ]
                        [ svg_
                            [ class_
                                "lucide lucide-book-open-icon lucide-book-open"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , strokeWidth_ "2"
                            , stroke_ "currentColor"
                            , fill_ "currentColor"
                            , viewBox_ "0 0 24 24"
                            , height_ "24"
                            , width_ "24"
                            , xmlns_ "http://www.w3.org/2000/svg"
                            ]
                            [ S.path_ [d_ "M12 7v14"]
                            , S.path_
                                [ d_
                                    "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                ]
                            ]
                        ]
                    , div_
                        [ class_
                            "grid flex-1 text-left text-sm leading-tight"
                        ]
                        [ span_
                            [class_ "truncate font-semibold"]
                            ["the miso book"]
                        , span_
                            [ class_ "truncate text-xs text-muted-foreground"
                            ]
                            ["v1.9.0"]
                        ]
                    ]
                ]
            , section_
                [class_ "scrollbar"]
                [ div_
                    [ aria_ "labelledby" "group-label-sidebar-content-1"
                    , role_ "group"
                    ]
                    [ h3_
                        [id_ "group-label-sidebar-content-1"]
                        ["Getting started"]
                    , ul_
                        []
                        [ li_
                            []
                            [ a_
                                [ onClick (GoTo Introduction)
                                ]
                                [ svg_
                                    [ strokeLinejoin_ "round"
                                    , strokeLinecap_ "round"
                                    , strokeWidth_ "2"
                                    , stroke_ "currentColor"
                                    , fill_ "none"
                                    , viewBox_ "0 0 24 24"
                                    , height_ "24"
                                    , width_ "24"
                                    , xmlns_ "http://www.w3.org/2000/svg"
                                    , class_ "lucide lucide-info lucide-info"
                                    ]
                                    [ circle_ [r_ "10", cy_ "12", cx_ "12"]
                                    , S.path_ [d_ "M12 16v-4"]
                                    , S.path_ [d_ "M12 8h.01"]
                                    ]
                                , span_ [] ["Introduction"]
                                ]
                            ]
                        , li_
                            []
                            [ a_
                                [ onClick (GoTo Install)
                                ]
                                [ svg_
                                    [ strokeLinejoin_ "round"
                                    , strokeLinecap_ "round"
                                    , strokeWidth_ "2"
                                    , stroke_ "currentColor"
                                    , fill_ "none"
                                    , viewBox_ "0 0 24 24"
                                    , height_ "24"
                                    , width_ "24"
                                    , xmlns_ "http://www.w3.org/2000/svg"
                                    , class_ "lucide lucide-package lucide-package"
                                    ]
                                    [ S.path_
                                        [ d_
                                            "M11 21.73a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73z"
                                        ]
                                    , S.path_ [d_ "M12 22V12"]
                                    , polyline_ [points_ "3.29 7 12 12 20.71 7"]
                                    , S.path_ [d_ "m7.5 4.27 9 5.15"]
                                    ]
                                , span_ [] ["Install"]
                                ]
                            ]
                        -- , li_
                        --     []
                        --     [ a_
                        --         [ onClick (GoTo Customize)
                        --         ]
                        --         [ svg_
                        --             [ strokeLinejoin_ "round"
                        --             , strokeLinecap_ "round"
                        --             , strokeWidth_ "2"
                        --             , stroke_ "currentColor"
                        --             , fill_ "none"
                        --             , viewBox_ "0 0 24 24"
                        --             , height_ "24"
                        --             , width_ "24"
                        --             , xmlns_ "http://www.w3.org/2000/svg"
                        --             , class_
                        --                 "lucide lucide-sliders-vertical lucide-sliders-vertical"
                        --             ]
                        --             [ S.path_ [d_ "M10 8h4"]
                        --             , S.path_ [d_ "M12 21v-9"]
                        --             , S.path_ [d_ "M12 8V3"]
                        --             , S.path_ [d_ "M17 16h4"]
                        --             , S.path_ [d_ "M19 12V3"]
                        --             , S.path_ [d_ "M19 21v-5"]
                        --             , S.path_ [d_ "M3 14h4"]
                        --             , S.path_ [d_ "M5 10V3"]
                        --             , S.path_ [d_ "M5 21v-7"]
                        --             ]
                        --         , span_ [] ["Customize"]
                        --         ]
                        --     ]
                        , li_
                            []
                            [ details_
                                [ textProp "open" ""
                                , id_ "submenu-sidebar-content-1-4"
                                ]
                                [ summary_
                                    [ aria_
                                        "controls"
                                        "submenu-sidebar-content-1-4-content"
                                    ]
                                    [ svg_
                                        [ strokeLinejoin_ "round"
                                        , strokeLinecap_ "round"
                                        , strokeWidth_ "2"
                                        , stroke_ "currentColor"
                                        , fill_ "none"
                                        , viewBox_ "0 0 24 24"
                                        , height_ "24"
                                        , width_ "24"
                                        , xmlns_ "http://www.w3.org/2000/svg"
                                        , class_ "lucide lucide-file-text lucide-file-text"
                                        ]
                                        [ S.path_
                                            [ d_
                                                "M6 22a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h8a2.4 2.4 0 0 1 1.704.706l3.588 3.588A2.4 2.4 0 0 1 20 8v12a2 2 0 0 1-2 2z"
                                            ]
                                        , S.path_ [d_ "M14 2v5a1 1 0 0 0 1 1h5"]
                                        , S.path_ [d_ "M10 9H8"]
                                        , S.path_ [d_ "M16 13H8"]
                                        , S.path_ [d_ "M16 17H8"]
                                        ]
                                    , "Chapters"
                                    ]
                                , ul_
                                    [id_ "submenu-sidebar-content-1-4-content"]
                                    [ li_
                                        []
                                        [ a_
                                            [ onClick (GoTo (Chapter (Capture 1)))
                                            ]
                                            [ span_ [] ["Chapter 1"]
                                            ]
                                        ]
                                    , li_
                                        []
                                        [ a_
                                          [ onClick (GoTo (Chapter (Capture 2)))
                                          ]
                                          [ span_ [] ["Chapter 2"]
                                          ]
                                        ]
                                    , li_
                                        []
                                        [ a_
                                            [ onClick (GoTo (Chapter (Capture 3)))
                                            ]
                                            [ span_ [] ["Chapter 3"]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , main_
        [id_ "content"]
        [ header_
            [ class_
                "bg-background sticky inset-x-0 top-0 isolate flex shrink-0 items-center gap-2 border-b z-10"
            ]
            [ dialog_
                [ aria_ "label" "Command menu"
                , class_ "command-dialog "
                , id_ "command-search"
                ]
                [ div_
                    [class_ "command"]
                    [ header_
                        []
                        [ svg_
                            [ class_ "lucide lucide-search-icon lucide-search"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , strokeWidth_ "2"
                            , stroke_ "currentColor"
                            , fill_ "none"
                            , viewBox_ "0 0 24 24"
                            , height_ "24"
                            , width_ "24"
                            , xmlns_ "http://www.w3.org/2000/svg"
                            ]
                            [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                            , S.path_ [d_ "m21 21-4.3-4.3"]
                            ]
                        , input_
                            [ aria_ "controls" "command-search-menu"
                            , aria_ "expanded" "true"
                            , role_ "combobox"
                            , aria_ "autocomplete" "list"
                            , spellcheck_ False
                            , autocorrect_ False
                            , autocomplete_ False
                            , placeholder_ "Search the docs..."
                            , id_ "command-search-input"
                            , type_ "text"
                            ]
                        ]
                    , div_
                        [ class_ "scrollbar "
                        , data_ "empty" "No results found."
                        , aria_ "orientation" "vertical"
                        , id_ "command-search-menu"
                        , role_ "menu"
                        ]
                        [ div_
                            [ aria_
                                "labelledby"
                                "group-label-command-search-items-1"
                            , role_ "group"
                            ]
                            [ span_
                                [ id_ "group-label-command-search-items-1"
                                , role_ "heading"
                                ]
                                ["Getting started"]
                            , div_
                                [ data_
                                    "keywords"
                                    "An introduction to web programming with Haskell miso"
                                , role_ "menuitem"
                                , id_ "command-search-items-1-1"
                                ]
                                [ svg_
                                    [ strokeLinejoin_ "round"
                                    , strokeLinecap_ "round"
                                    , strokeWidth_ "2"
                                    , stroke_ "currentColor"
                                    , fill_ "none"
                                    , viewBox_ "0 0 24 24"
                                    , height_ "24"
                                    , width_ "24"
                                    , xmlns_ "http://www.w3.org/2000/svg"
                                    , class_ "lucide lucide-info lucide-info"
                                    ]
                                    [ circle_ [r_ "10", cy_ "12", cx_ "12"]
                                    , S.path_ [d_ "M12 16v-4"]
                                    , S.path_ [d_ "M12 8h.01"]
                                    ]
                                , span_ [] ["Introduction"]
                                ]
                            , div_
                                [ data_
                                    "keywords"
                                    "Install Create a new docs site and deploy it."
                                , role_ "menuitem"
                                , id_ "command-search-items-1-2"
                                ]
                                [ svg_
                                    [ strokeLinejoin_ "round"
                                    , strokeLinecap_ "round"
                                    , strokeWidth_ "2"
                                    , stroke_ "currentColor"
                                    , fill_ "none"
                                    , viewBox_ "0 0 24 24"
                                    , height_ "24"
                                    , width_ "24"
                                    , xmlns_ "http://www.w3.org/2000/svg"
                                    , class_ "lucide lucide-package lucide-package"
                                    ]
                                    [ S.path_
                                        [ d_
                                            "M11 21.73a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73z"
                                        ]
                                    , S.path_ [d_ "M12 22V12"]
                                    , polyline_ [points_ "3.29 7 12 12 20.71 7"]
                                    , S.path_ [d_ "m7.5 4.27 9 5.15"]
                                    ]
                                , span_ [] ["Install"]
                                ]
                            , div_
                                [ data_
                                    "keywords"
                                    "Customize Configure the site, templates, and styles."
                                , role_ "menuitem"
                                , id_ "command-search-items-1-3"
                                ]
                                [ svg_
                                    [ strokeLinejoin_ "round"
                                    , strokeLinecap_ "round"
                                    , strokeWidth_ "2"
                                    , stroke_ "currentColor"
                                    , fill_ "none"
                                    , viewBox_ "0 0 24 24"
                                    , height_ "24"
                                    , width_ "24"
                                    , xmlns_ "http://www.w3.org/2000/svg"
                                    , class_
                                        "lucide lucide-sliders-vertical lucide-sliders-vertical"
                                    ]
                                    [ S.path_ [d_ "M10 8h4"]
                                    , S.path_ [d_ "M12 21v-9"]
                                    , S.path_ [d_ "M12 8V3"]
                                    , S.path_ [d_ "M17 16h4"]
                                    , S.path_ [d_ "M19 12V3"]
                                    , S.path_ [d_ "M19 21v-5"]
                                    , S.path_ [d_ "M3 14h4"]
                                    , S.path_ [d_ "M5 10V3"]
                                    , S.path_ [d_ "M5 21v-7"]
                                    ]
                                , span_ [] ["Customize"]
                                ]
                            , div_
                                [ aria_
                                    "labelledby"
                                    "group-label-command-search-items-1-4"
                                , role_ "group"
                                ]
                                [ span_
                                    [ id_ "group-label-command-search-items-1-4"
                                    , role_ "heading"
                                    ]
                                    ["Manage content"]
                                , div_
                                    [ data_
                                        "keywords"
                                        "Navigation Configure groups, items, and submenus in `docs/docs.json`."
                                    , role_ "menuitem"
                                    , id_ "command-search-items-1-4-1"
                                    ]
                                    [span_ [] ["Navigation"]]
                                , div_
                                    [ data_
                                        "keywords"
                                        "Pages Frontmatter, Markdown, code blocks, media, and Basecoat components."
                                    , role_ "menuitem"
                                    , id_ "command-search-items-1-4-2"
                                    ]
                                    [span_ [] ["Pages"]]
                                , div_
                                    [ data_
                                        "keywords"
                                        "CMS Use Pages CMS to edit docs and navigation in user-friendly UI."
                                    , role_ "menuitem"
                                    , id_ "command-search-items-1-4-3"
                                    ]
                                    [span_ [] ["CMS"]]
                                ]
                            ]
                        ]
                    , button_
                        [ aria_ "label" "Close dialog"
                        , type_ "button"
                        ]
                        [ svg_
                            [ class_ "lucide lucide-x-icon lucide-x"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , strokeWidth_ "2"
                            , stroke_ "currentColor"
                            , fill_ "none"
                            , viewBox_ "0 0 24 24"
                            , height_ "24"
                            , width_ "24"
                            , xmlns_ "http://www.w3.org/2000/svg"
                            ]
                            [ S.path_ [d_ "M18 6 6 18"]
                            , S.path_ [d_ "m6 6 12 12"]
                            ]
                        ]
                    ]
                ]
            , div_
                [ class_
                    "flex h-14 w-full items-center justify-between gap-2 px-4"
                ]
                [ button_
                    [ class_ "btn-sm-icon-ghost"
                    , data_ "align" "start"
                    , data_ "side" "bottom"
                    , data_ "tooltip" "Toggle sidebar"
                    , aria_ "label" "Toggle sidebar"
                    , type_ "button"
                    , onClick ToggleSidebar
                    ]
                    [ svg_
                        [ strokeLinejoin_ "round"
                        , strokeLinecap_ "round"
                        , strokeWidth_ "2"
                        , stroke_ "currentColor"
                        , fill_ "none"
                        , viewBox_ "0 0 24 24"
                        , height_ "24"
                        , width_ "24"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ rect_
                            [ rx_ "2"
                            , y_ "3"
                            , x_ "3"
                            , height_ "18"
                            , width_ "18"
                            ]
                        , S.path_ [d_ "M9 3v18"]
                        ]
                    ]
                , button_
                    [ textProp "onclick" ""
                    , class_
                        "relative text-muted-foreground bg-background dark:bg-input/30 border-input flex items-center h-8 w-full min-w-0 rounded-md border px-3 py-1 gap-x-2 text-base md:text-sm focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] outline-none cursor-text max-w-72 sm:ml-auto"
                    , type_ "button"
                    ]
                    [ svg_
                        [ class_ "size-4"
                        , strokeLinejoin_ "round"
                        , strokeLinecap_ "round"
                        , strokeWidth_ "2"
                        , stroke_ "currentColor"
                        , fill_ "none"
                        , viewBox_ "0 0 24 24"
                        , height_ "24"
                        , width_ "24"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ S.path_ [d_ "m21 21-4.34-4.34"]
                        , circle_ [r_ "8", cy_ "11", cx_ "11"]
                        ]
                    , "Search the docs..."
                    , kbd_ [class_ "kbd ml-auto -mr-1.5"] ["⌘K"]
                    ]
                , nav_
                    [class_ "items-center gap-2 hidden sm:flex"]
                    [ a_
                        [ target_ "_blank"
                        , rel_ "noopener"
                        , data_ "side" "bottom"
                        , data_ "tooltip" "GitHub"
                        , aria_ "label" "GitHub"
                        , P.href_
                            "https://github.com/haskell-miso/miso-book"
                        , class_ "btn-sm h-7"
                        ]
                        [ svg_
                            [ xmlns_ "http://www.w3.org/2000/svg"
                            , viewBox_ "0 0 24 24"
                            , fill_ "currentColor"
                            , role_ "img"
                            ]
                            [ title_ [] ["GitHub"]
                            , S.path_
                                [ d_
                                    "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
                                ]
                            ]
                        , "GitHub"
                        ]
                    , button_
                        [ class_ "btn-sm-icon-outline size-7"
                        , textProp "onclick" ""
                        , data_ "align" "end"
                        , data_ "side" "bottom"
                        , data_ "tooltip" "Toggle dark mode"
                        , aria_ "label" "Toggle dark mode"
                        , type_ "button"
                        , onClickCapture ToggleDarkMode
                        ]
                        [ span_
                            [class_ "hidden dark:block"]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ circle_ [r_ "4", cy_ "12", cx_ "12"]
                                , S.path_ [d_ "M12 2v2"]
                                , S.path_ [d_ "M12 20v2"]
                                , S.path_ [d_ "m4.93 4.93 1.41 1.41"]
                                , S.path_ [d_ "m17.66 17.66 1.41 1.41"]
                                , S.path_ [d_ "M2 12h2"]
                                , S.path_ [d_ "M20 12h2"]
                                , S.path_ [d_ "m6.34 17.66-1.41 1.41"]
                                , S.path_ [d_ "m19.07 4.93-1.41 1.41"]
                                ]
                            ]
                        , span_
                            [class_ "block dark:hidden"]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ S.path_
                                    [ d_
                                        "M20.985 12.486a9 9 0 1 1-9.473-9.472c.405-.022.617.46.402.803a6 6 0 0 0 8.268 8.268c.344-.215.825-.004.803.401"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div_
                    [class_ "sm:hidden"]
                    [ button_
                        [ aria_ "controls" "mobile-menu"
                        , aria_ "expanded" "false"
                        , aria_ "label" "Toggle menu"
                        , class_ "btn-sm-icon-ghost peer group"
                        , type_ "button"
                        , textProp "onclick" "this.setAttribute('aria-expanded', this.getAttribute('aria-expanded') === 'true' ? 'false' : 'true')"
                        ]
                        [ span_
                            [class_ "group-aria-expanded:hidden"]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ S.path_ [d_ "M4 5h16"]
                                , S.path_ [d_ "M4 12h16"]
                                , S.path_ [d_ "M4 19h16"]
                                ]
                            ]
                        , span_
                            [class_ "hidden group-aria-expanded:block"]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ S.path_ [d_ "M18 6 6 18"]
                                , S.path_ [d_ "m6 6 12 12"]
                                ]
                            ]
                        ]
                    , nav_
                        [ data_ "mobile-menu" ""
                        , class_
                            "fixed inset-0 top-14 bg-background z-50 flex flex-col gap-2 p-4 transition-all opacity-0 scale-95 -translate-y-16 invisible pointer-events-none peer-aria-expanded:opacity-100 peer-aria-expanded:scale-100 peer-aria-expanded:translate-y-0 peer-aria-expanded:visible peer-aria-expanded:pointer-events-auto"
                        , id_ "mobile-menu"
                        ]
                        [ a_
                            [ target_ "_blank"
                            , rel_ "noopener"
                            , P.href_
                                "https://github.com/haskell-miso/miso-book"
                            , class_ "btn-lg-ghost justify-between "
                            ]
                            [ span_ [] ["GitHub"]
                            , svg_
                                [ xmlns_ "http://www.w3.org/2000/svg"
                                , viewBox_ "0 0 24 24"
                                , fill_ "currentColor"
                                , role_ "img"
                                ]
                                [ title_ [] ["GitHub"]
                                , S.path_
                                    [ d_
                                        "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div_
            [class_ "p-4 md:p-6 xl:p-12 flex gap-10"]
            [ article_
                [class_ "flex-1 min-w-0 wrap-break-word"]
                [ div_
                    [ class_
                        "mx-auto w-full max-w-2xl flex-1 space-y-10"
                    ]
                    [ header_
                        [class_ "space-y-2"]
                        [ div_
                            [class_ "flex items-start gap-x-2"]
                            [ h1_
                                [ class_ "text-3xl font-semibold tracking-tight sm:text-4xl mr-auto"
                                ]
                                ["Introduction"]
                            , div_
                                [class_ "flex items-center gap-x-2 mt-1.5"]
                                [ div_
                                    [class_ "button-group", role_ "group"]
                                    [ button_
                                        [ data_ "md-url" "/index.md"
                                        , class_
                                            "btn-sm-secondary h-7 px-2.5 text-[13px] max-sm:size-7"
                                        , type_ "button"
                                        ]
                                        [ svg_
                                            [ strokeLinejoin_ "round"
                                            , strokeLinecap_ "round"
                                            , strokeWidth_ "2"
                                            , stroke_ "currentColor"
                                            , fill_ "none"
                                            , viewBox_ "0 0 24 24"
                                            , height_ "24"
                                            , width_ "24"
                                            , xmlns_ "http://www.w3.org/2000/svg"
                                            ]
                                            [ rect_
                                                [ ry_ "2"
                                                , rx_ "2"
                                                , y_ "8"
                                                , x_ "8"
                                                , height_ "14"
                                                , width_ "14"
                                                ]
                                            , S.path_
                                                [ d_
                                                    "M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"
                                                ]
                                            ]
                                        , span_ [class_ "hidden sm:block"] ["Copy page"]
                                        ]
                                    , hr_ [role_ "separator"]
                                    , div_
                                        [ class_ "dropdown-menu "
                                        , id_ "dropdown-menu-358381"
                                        ]
                                        [ button_
                                            [ class_ "btn-sm-icon-secondary size-7"
                                            , aria_ "expanded" "false"
                                            , aria_ "controls" "dropdown-menu-358381-menu"
                                            , aria_ "haspopup" "menu"
                                            , id_ "dropdown-menu-358381-trigger"
                                            , type_ "button"
                                            ]
                                            [ svg_
                                                [ strokeLinejoin_ "round"
                                                , strokeLinecap_ "round"
                                                , strokeWidth_ "2"
                                                , stroke_ "currentColor"
                                                , fill_ "none"
                                                , viewBox_ "0 0 24 24"
                                                , height_ "24"
                                                , width_ "24"
                                                , xmlns_ "http://www.w3.org/2000/svg"
                                                ]
                                                [S.path_ [d_ "m6 9 6 6 6-6"]]
                                            ]
                                        , div_
                                            [ data_ "align" "end"
                                            , aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "dropdown-menu-358381-popover"
                                            ]
                                            [ div_
                                                [ aria_ "labelledby" "dropdown-menu-358381-trigger"
                                                , id_ "dropdown-menu-358381-menu"
                                                , role_ "menu"
                                                ]
                                                [ a_
                                                    [ rel_ "noopener"
                                                    , target_ "_blank"
                                                    , P.href_ "/index.md"
                                                    , role_ "menuitem"
                                                    , id_ "dropdown-menu-358381-items-1"
                                                    ]
                                                    [ svg_
                                                        [viewBox_ "0 0 22 16", strokeLinejoin_ "round"]
                                                        [ S.path_
                                                            [ fill_ "currentColor"
                                                            , d_
                                                                "M19.5 2.25H2.5C1.80964 2.25 1.25 2.80964 1.25 3.5V12.5C1.25 13.1904 1.80964 13.75 2.5 13.75H19.5C20.1904 13.75 20.75 13.1904 20.75 12.5V3.5C20.75 2.80964 20.1904 2.25 19.5 2.25ZM2.5 1C1.11929 1 0 2.11929 0 3.5V12.5C0 13.8807 1.11929 15 2.5 15H19.5C20.8807 15 22 13.8807 22 12.5V3.5C22 2.11929 20.8807 1 19.5 1H2.5ZM3 4.5H4H4.25H4.6899L4.98715 4.82428L7 7.02011L9.01285 4.82428L9.3101 4.5H9.75H10H11V5.5V11.5H9V7.79807L7.73715 9.17572L7 9.97989L6.26285 9.17572L5 7.79807V11.5H3V5.5V4.5ZM15 8V4.5H17V8H19.5L17 10.5L16 11.5L15 10.5L12.5 8H15Z"
                                                            , textProp "clip-rule" "evenodd"
                                                            , textProp "fill-rule" "evenodd"
                                                            ]
                                                        ]
                                                    , "View as Markdown"
                                                    ]
                                                ,
                                                  a_
                                                    [ rel_ "noopener"
                                                    , target_ "_blank"
                                                    -- , P.href_
                                                    --     "https://chatgpt.com/?q=Read%20from%20https%3A%2F%2Fmiso.com%2F%20so%20I%20can%20ask%20questions%20about%20it."
                                                    , role_ "menuitem"
                                                    , id_ "dropdown-menu-358381-items-2"
                                                    ]
                                                    [ svg_
                                                        [ viewBox_ "0 0 24 24"
                                                        , xmlns_ "http://www.w3.org/2000/svg"
                                                        ]
                                                        [ S.path_
                                                            [ fill_ "currentColor"
                                                            , d_
                                                                "M22.282 9.821a5.985 5.985 0 0 0-.516-4.91 6.046 6.046 0 0 0-6.51-2.9A6.065 6.065 0 0 0 4.981 4.18a5.985 5.985 0 0 0-3.998 2.9 6.046 6.046 0 0 0 .743 7.097 5.98 5.98 0 0 0 .51 4.911 6.051 6.051 0 0 0 6.515 2.9A5.985 5.985 0 0 0 13.26 24a6.056 6.056 0 0 0 5.772-4.206 5.99 5.99 0 0 0 3.997-2.9 6.056 6.056 0 0 0-.747-7.073zM13.26 22.43a4.476 4.476 0 0 1-2.876-1.04l.141-.081 4.779-2.758a.795.795 0 0 0 .392-.681v-6.737l2.02 1.168a.071.071 0 0 1 .038.052v5.583a4.504 4.504 0 0 1-4.494 4.494zM3.6 18.304a4.47 4.47 0 0 1-.535-3.014l.142.085 4.783 2.759a.771.771 0 0 0 .78 0l5.843-3.369v2.332a.08.08 0 0 1-.033.062L9.74 19.95a4.5 4.5 0 0 1-6.14-1.646zM2.34 7.896a4.485 4.485 0 0 1 2.366-1.973V11.6a.766.766 0 0 0 .388.676l5.815 3.355-2.02 1.168a.076.076 0 0 1-.071 0l-4.83-2.786A4.504 4.504 0 0 1 2.34 7.872zm16.597 3.855-5.833-3.387L15.119 7.2a.076.076 0 0 1 .071 0l4.83 2.791a4.494 4.494 0 0 1-.676 8.105v-5.678a.79.79 0 0 0-.407-.667zm2.01-3.023-.141-.085-4.774-2.782a.776.776 0 0 0-.785 0L9.409 9.23V6.897a.066.066 0 0 1 .028-.061l4.83-2.787a4.5 4.5 0 0 1 6.68 4.66zm-12.64 4.135-2.02-1.164a.08.08 0 0 1-.038-.057V6.075a4.5 4.5 0 0 1 7.375-3.453l-.142.08-4.778 2.758a.795.795 0 0 0-.393.681zm1.097-2.365 2.602-1.5 2.607 1.5v2.999l-2.597 1.5-2.607-1.5Z"
                                                            ]
                                                        ]
                                                    , "Open in ChatGPT"
                                                    ]
                                                , a_
                                                    [ rel_ "noopener"
                                                    , target_ "_blank"
                                                    , P.href_
                                                        "https://claude.ai/new?q=Read%20from%20https%3A%2F%2Fmiso.com%2F%20so%20I%20can%20ask%20questions%20about%20it."
                                                    , role_ "menuitem"
                                                    , id_ "dropdown-menu-358381-items-3"
                                                    ]
                                                    [ svg_
                                                        [ viewBox_ "0 0 24 24"
                                                        , xmlns_ "http://www.w3.org/2000/svg"
                                                        ]
                                                        [ S.path_
                                                            [ fill_ "currentColor"
                                                            , d_
                                                                "m4.714 15.956 4.718-2.648.079-.23-.08-.128h-.23l-.79-.048-2.695-.073-2.337-.097-2.265-.122-.57-.121-.535-.704.055-.353.48-.321.685.06 1.518.104 2.277.157 1.651.098 2.447.255h.389l.054-.158-.133-.097-.103-.098-2.356-1.596-2.55-1.688-1.336-.972-.722-.491L2 6.223l-.158-1.008.655-.722.88.06.225.061.893.686 1.906 1.476 2.49 1.833.364.304.146-.104.018-.072-.164-.274-1.354-2.446-1.445-2.49-.644-1.032-.17-.619a2.972 2.972 0 0 1-.103-.729L6.287.133 6.7 0l.995.134.42.364.619 1.415L9.735 4.14l1.555 3.03.455.898.243.832.09.255h.159V9.01l.127-1.706.237-2.095.23-2.695.08-.76.376-.91.747-.492.583.28.48.685-.067.444-.286 1.851-.558 2.903-.365 1.942h.213l.243-.242.983-1.306 1.652-2.064.728-.82.85-.904.547-.431h1.032l.759 1.129-.34 1.166-1.063 1.347-.88 1.142-1.263 1.7-.79 1.36.074.11.188-.02 2.853-.606 1.542-.28 1.84-.315.832.388.09.395-.327.807-1.967.486-2.307.462-3.436.813-.043.03.049.061 1.548.146.662.036h1.62l3.018.225.79.522.473.638-.08.485-1.213.62-1.64-.389-3.825-.91-1.31-.329h-.183v.11l1.093 1.068 2.003 1.81 2.508 2.33.127.578-.321.455-.34-.049-2.204-1.657-.85-.747-1.925-1.62h-.127v.17l.443.649 2.343 3.521.122 1.08-.17.353-.607.213-.668-.122-1.372-1.924-1.415-2.168-1.141-1.943-.14.08-.674 7.254-.316.37-.728.28-.607-.461-.322-.747.322-1.476.388-1.924.316-1.53.285-1.9.17-.632-.012-.042-.14.018-1.432 1.967-2.18 2.945-1.724 1.845-.413.164-.716-.37.066-.662.401-.589 2.386-3.036 1.439-1.882.929-1.086-.006-.158h-.055L4.138 18.56l-1.13.146-.485-.456.06-.746.231-.243 1.907-1.312Z"
                                                            ]
                                                        ]
                                                    , "Open in Claude"
                                                    ]
                                                , a_
                                                    [ rel_ "noopener"
                                                    , target_ "_blank"
                                                    , P.href_
                                                        "cursor://anysphere.cursor-deeplink/prompt?text=Read%20from%20https%3A%2F%2Fmiso.com%2F%20so%20I%20can%20ask%20questions%20about%20it."
                                                    , role_ "menuitem"
                                                    , id_ "dropdown-menu-358381-items-4"
                                                    ]
                                                    [ svg_
                                                        [ xmlns_ "http://www.w3.org/2000/svg"
                                                        , viewBox_ "0 0 24 24"
                                                        , role_ "img"
                                                        ]
                                                        [ title_ [] ["Cursor"]
                                                        , S.path_
                                                            [ fill_ "currentColor"
                                                            , d_
                                                                "M11.503.131 1.891 5.678a.84.84 0 0 0-.42.726v11.188c0 .3.162.575.42.724l9.609 5.55a1 1 0 0 0 .998 0l9.61-5.55a.84.84 0 0 0 .42-.724V6.404a.84.84 0 0 0-.42-.726L12.497.131a1.01 1.01 0 0 0-.996 0M2.657 6.338h18.55c.263 0 .43.287.297.515L12.23 22.918c-.062.107-.229.064-.229-.06V12.335a.59.59 0 0 0-.295-.51l-9.11-5.257c-.109-.063-.064-.23.061-.23"
                                                            ]
                                                        ]
                                                    , "Open in Cursor"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                , div_
                                    [class_ "button-group", role_ "group"]
                                    [ a_
                                        [ class_
                                            "btn-sm-icon-secondary hidden sm:flex size-7"
                                        , P.href_ "/install/"
                                        , onClickPrevent (GoTo Install)
                                        ]
                                        [ span_ [class_ "sr-only"] ["Install"]
                                        , svg_
                                            [ strokeLinejoin_ "round"
                                            , strokeLinecap_ "round"
                                            , strokeWidth_ "2"
                                            , stroke_ "currentColor"
                                            , fill_ "none"
                                            , viewBox_ "0 0 24 24"
                                            , height_ "24"
                                            , width_ "24"
                                            , xmlns_ "http://www.w3.org/2000/svg"
                                            ]
                                            [ S.path_ [d_ "M5 12h14"]
                                            , S.path_ [d_ "m12 5 7 7-7 7"]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , p_
                            [class_ "text-muted-foreground sm:text-base"]
                            [ "The Haskell miso book 📖 🍜 "
                            ]
                        ]
                    , div_
                        [class_ "prose"]
                        [ h2_
                            [tabindex_ "-1", id_ "why-miso%3F"]
                            [ a_
                                [ P.href_ "#why-miso%3F"
                                , class_ "header-anchor"
                                ]
                                [ case route uri of
                                    Right Index -> "home"
                                    Right Introduction -> "introduction"
                                    Right Install -> "install"
                                    Right (Chapter (Capture n)) -> text ("chapter " <> ms n)
                                    _ -> "oops"
                                ]
                            ]
                        ]
--                        , p_
                            -- []
                            -- [ "There are plenty of existing solutions to create documentation websites: Mintlify, Fumadocs, Docusaurus, MkDocs... So why create another one?"
                            -- ]
                        -- , p_
                        --     []
                        --     [ "I wanted something that was fast, modern, simple, and good-looking. In particular, I did not want any React."
                        --     ]
                        -- , p_
                        --     []
                        --     [ "So I built Miso with"
                        --     , a_ [P.href_ "https://11ty.dev"] ["11ty"]
                        --     , "and"
                        --     , a_ [P.href_ "https://basecoatui.com"] ["Basecoat"]
                        --     , "(Tailwind-based). You get vanilla HTML and CSS, with a tiny bit of JS. And it uses the same design system as shadcn/ui."
                        --     ]
                        -- , h2_
                        --     [tabindex_ "-1", id_ "key-features"]
                        --     [ a_
                        --         [P.href_ "#key-features", class_ "header-anchor"]
                        --         ["Key features"]
                        --     ]
                        -- , ul_
                        --     []
                        --     [ li_
                        --         []
                        --         [ strong_ [] ["Fast"]
                        --         , ": 11ty is one of the fastest site generators, usually only second to Hugo."
                        --         ]
                        --     , li_
                        --         []
                        --         [ strong_ [] ["Standard and Reliable"]
                        --         , ": It's all vanilla CSS, JS and HTML. No complex frameworks with hydration errors."
                        --         ]
                        --     , li_
                        --         []
                        --         [ strong_ [] ["Git-based with a user-friendly CMS"]
                        --         , ": All of your content lives in a GitHub repo. And if you need a user-friendly interface for your teammates to edit the content, just use"
                        --         , a_ [P.href_ "https://pagescms.org"] ["Pages CMS"]
                        --         , "."
                        --         ]
                        --     , li_
                        --         []
                        --         [ strong_ [] ["Markdown + Nunjucks"]
                        --         , ": Simple Markdown YAML frontmatter, with the convenience of Nunjucks templating if you need more advance features (e.g. loading Lucide icons)."
                        --         ]
                        --     , li_
                        --         []
                        --         [ strong_ [] ["LLM-friendly"]
                        --         , ": Auto-generated"
                        --         , code_ [] ["llms.txt"]
                        --         , ","
                        --         , code_ [] ["llms-full.txt"]
                        --         , ", and per-page"
                        --         , code_ [] ["/*.md"]
                        --         , "exports for LLMs."
                        --         ]
                        --     , li_
                        --         []
                        --         [ strong_ [] ["100% free and open source"]
                        --         , ": I have nothing to sell. No hosting plan, no advanced features. You can host it for free on Cloudflare Pages."
                        --         ]
                        --     ]
                        -- , h2_
                        --     [tabindex_ "-1", id_ "how-can-i-help%3F"]
                        --     [ a_
                        --         [ P.href_ "#how-can-i-help%3F"
                        --         , class_ "header-anchor"
                        --         ]
                        --         ["How can I help?"]
                        --     ]
                        -- , p_
                        --     []
                        --     ["Miso is 100% open source and free."]
                        -- , ul_
                        --     []
                        --     [ li_
                        --         []
                        --         [ a_
                        --             [ P.href_
                        --                 "https://github.com/dmjio/miso"
                        --             ]
                        --             ["Star it on GitHub"]
                        --         ]
                        --     , li_
                        --         []
                        --         [ a_
                        --             [ P.href_
                        --                 "https://github.com/haskell-miso/miso/issues"
                        --             ]
                        --             ["Report bugs or request features"]
                        --         ]
                        --     , li_
                        --         []
                        --         [ a_
                        --             [ P.href_
                        --                 "https://github.com/haskell-miso/miso/pulls"
                        --             ]
                        --             ["Submit a pull request"]
                        --         ]
                        --     , li_
                        --         []
                        --         [ a_
                        --             [P.href_ "https://github.com/sponsors/dmjio"]
                        --             ["Sponsor the project"]
                        --         ]
                        --     ]
                        -- ]
                    -- , footer_
                    --     [class_ "flex items-center mt-12"]
                    --     [ a_
                    --         [ class_ "btn-sm-secondary ml-auto"
                    --         , P.href_ "/install/"
                    --         ]
                    --         [ "Install"
                    --         , svg_
                    --             [ strokeLinejoin_ "round"
                    --             , strokeLinecap_ "round"
                    --             , strokeWidth_ "2"
                    --             , stroke_ "currentColor"
                    --             , fill_ "none"
                    --             , viewBox_ "0 0 24 24"
                    --             , height_ "24"
                    --             , width_ "24"
                    --             , xmlns_ "http://www.w3.org/2000/svg"
                    --             ]
                    --             [ S.path_ [d_ "M5 12h14"]
                    --             , S.path_ [d_ "m12 5 7 7-7 7"]
                    --             ]
                    --         ]
                    --     ]
                    -- ]
                    ]
                ]
            , aside_
                [class_ "hidden xl:block w-56"]
                [ div_
                    [ class_
                        "sticky top-20 text-sm [&_a]:text-muted-foreground [&_a]:hover:text-primary space-y-2 [&_ol]:space-y-2 [&_ol_ol]:pl-3 [&_ol_ol]:mt-2"
                    ]
                    [ h2_
                        [class_ "font-medium flex items-center gap-1"]
                        [ svg_
                            [ class_ "size-4"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , strokeWidth_ "2"
                            , stroke_ "currentColor"
                            , fill_ "none"
                            , viewBox_ "0 0 24 24"
                            , height_ "24"
                            , width_ "24"
                            , xmlns_ "http://www.w3.org/2000/svg"
                            ]
                            [ S.path_ [d_ "M16 5H3"]
                            , S.path_ [d_ "M16 12H3"]
                            , S.path_ [d_ "M16 19H3"]
                            , S.path_ [d_ "M21 5h.01"]
                            , S.path_ [d_ "M21 12h.01"]
                            , S.path_ [d_ "M21 19h.01"]
                            ]
                        , "On this page"
                        ]
                    , nav_
                        [class_ "toc"]
                        [ ol_
                            []
                            [ li_
                                []
                                [ a_
                                    [ onClick (GoTo Index)
                                    ]
                                    ["The miso book"]
                                ]
                            , li_
                                []
                                [a_ [P.href_ "#key-features"] ["Key features"]]
                            , li_
                                []
                                [ a_
                                    [P.href_ "#how-can-i-help%3F"]
                                    ["How can I help?"]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
       ]
    ]
    
