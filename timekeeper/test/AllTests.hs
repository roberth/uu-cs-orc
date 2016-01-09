{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import {-@ HTF_TESTS @-} Network.TimeKeeper.ServerTest
main = htfMain htf_importedTests
