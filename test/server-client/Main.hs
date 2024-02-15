{-# LANGUAGE OverloadedLists #-}
module Main (main) where

import Netw.Socket
import Netw.SockAddr

import Control.Monad

import Data.Function
import Data.Bits
import Data.Primitive

import System.Exit
import System.Posix

serverMain :: FilePath -> IO ()
serverMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  bind server addr
  listen server 1
  
  client <- accept_ server
  fix (\ again -> do
    msg <- recvsome client 128 zeroBits
    if sizeofByteArray msg > 0
    then do
      putStrLn ("SERVER RECEIVED " ++ show (sizeofByteArray msg) ++ " BYTES")
      void (sendall client msg zeroBits)
      again
    else do
      putStrLn "SERVER EXIT"
      return ()
    )
  closeSocket client
  
  removeLink sunPath
  closeSocket server
  where addr = SockAddrUn sunPath
        

clientMain :: FilePath -> IO ()
clientMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect server addr
  void (sendall server [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] zeroBits)
  msg <- recvsome server 128 zeroBits 
  putStrLn ("CLIENT RECEIVED " ++ show (sizeofByteArray msg) ++ " BYTES")
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
