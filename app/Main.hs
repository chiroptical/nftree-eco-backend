module Main where

import NftreeEcoBackend

main :: IO ()
main =
    print $ "Hello from " ++ doNftreeEcoBackend ++ "!"
