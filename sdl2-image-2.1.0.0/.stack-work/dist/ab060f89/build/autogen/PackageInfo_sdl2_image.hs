{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_sdl2_image (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "sdl2_image"
version :: Version
version = Version [2,1,0,0] []

synopsis :: String
synopsis = "Haskell bindings to SDL2_image"
copyright :: String
copyright = "2014 Cal Lei,\n2015 Sini\353a Bi\273in,\n2021 Daniel Firth"
homepage :: String
homepage = ""
