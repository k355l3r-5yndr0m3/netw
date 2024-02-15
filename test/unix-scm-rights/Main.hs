{-# LANGUAGE OverloadedLists #-}
module Main (main) where
import System.Posix.IO

import System.Posix.Process

import Netw.Socket
import Netw.SockAddr
import Netw.Ancillary

import Control.Monad

import Data.Bits
import Data.List
import Data.Primitive

import System.Posix
import System.Exit

serverMain :: FilePath -> IO ()
serverMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  bind server addr
  listen server 1
  
  client <- accept_ server
  buffer <- newByteArray 8
  
  (_, _, ancil) <- recvmsg_ client [(buffer, 0, 8)] 64 zeroBits
  let x :: Maybe ScmRights = (\ a -> ancillaryData a) . fst =<< uncons ancil
      y = case x of
            Nothing ->
              putStrLn "Failed to recv ancillary data"
            Just (ScmRights fds) -> do
              putStrLn "Received ancillary data"
              print fds
  y

  closeSocket client
  removeLink sunPath
  closeSocket server
  where addr = SockAddrUn sunPath
        

clientMain :: FilePath -> IO ()
clientMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect server addr
  fd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
  void (sendmsg server SockAddrNull [([0], 0, 1)] [mkAncillaryData (ScmRights [fd])] zeroBits)
  closeSocket server
  where addr = SockAddrUn sunPath

main :: IO ()
main = do
  s <- forkProcess (serverMain sunPath)
  c <- forkProcess (clientMain sunPath)

  serverExit <- maybe (fail "Server failure") return =<< getProcessStatus True True s
  clientExit <- maybe (fail "Client failure") return =<< getProcessStatus True True c

  case (serverExit, clientExit) of
    (Exited ExitSuccess, Exited ExitSuccess) -> exitSuccess
    _otherwise -> exitFailure

  where sunPath = "/run/user/1000/test"
