module Main where

import Relude
import Server
import Snap.Http.Server

main :: IO ()
main = quickHttpServe routes
