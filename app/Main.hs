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
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics
import           Control.Monad
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
import           Types
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
withLocalJS :: IO a -> IO ()
withLocalJS action = void $ do
#ifdef WASM
  $(evalFile "js/util.js")
#endif
  action
-----------------------------------------------------------------------------
main :: IO ()
main = miso defaultEvents app
-----------------------------------------------------------------------------
