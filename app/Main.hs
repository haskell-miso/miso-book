-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.Lens
import           Miso.String
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp app
-----------------------------------------------------------------------------
app :: App () action
app = component () noop $ \() -> "The Haskell Miso book"
-----------------------------------------------------------------------------
