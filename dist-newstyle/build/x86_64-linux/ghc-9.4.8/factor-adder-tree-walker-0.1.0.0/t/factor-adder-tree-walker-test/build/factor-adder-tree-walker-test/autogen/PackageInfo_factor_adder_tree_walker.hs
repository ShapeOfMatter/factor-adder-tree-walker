{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_factor_adder_tree_walker (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "factor_adder_tree_walker"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Brute force search for exceptions to https://www.youtube.com/watch?v=pylw9t4j6bM"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
