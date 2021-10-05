{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Hedgehog
import           Hedgehog.Main
import           NftreeEcoBackend

prop_test :: Property
prop_test = property $ do
    doNftreeEcoBackend === "NftreeEcoBackend"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
