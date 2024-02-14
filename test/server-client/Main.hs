module Main (main) where

import Server
import Client

import System.Posix.Process
import Control.Concurrent

main :: IO ()
main = do
  _ <- forkProcess (serverMain sunPath)
  _ <- forkProcess (clientMain sunPath)

  threadDelay 5000000
  return ()
  where sunPath = "/run/user/1000/test"
