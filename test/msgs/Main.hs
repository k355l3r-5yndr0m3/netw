{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where
import Netw.Socket
import Netw.SockAddr

import Control.Monad

import Data.Bits
import Data.Word
import Data.Primitive

import System.Posix

import System.Exit

clientMain :: FilePath -> IO ()
clientMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect server addr
  
  void (sendmsg server SockAddrNull [(msg, 0, sizeofByteArray msg)] [] zeroBits)

  closeSocket server
  where addr = SockAddrUn sunPath
        msg  = [0, 1, 2, 3, 4, 5, 6, 7]

serverMain :: FilePath -> IO ()
serverMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  bind server addr
  listen server 1
  
  client <- accept_ server
  buffer <- newByteArray 128
  (bytesRecv, _, _) <- recvmsg_ client [(buffer, 0, 128)] 0 zeroBits

  putStrLn $ "SERVER RECEIVED " ++ show bytesRecv ++ " BYTES"
  shrinkMutableByteArray buffer bytesRecv
  buffer' <- unsafeFreezeByteArray buffer
  printByteArray buffer'

  closeSocket client
  
  removeLink sunPath
  closeSocket server
  where addr = SockAddrUn sunPath

printByteArray :: ByteArray -> IO ()
printByteArray ba = putStr str
  where str = foldrByteArray (\ (hex -> (h, l)) s -> '\\':'x':h:l:s) "" ba
        hex :: Word8 -> (Char, Char)
        hex w =
          let hex' = \case 0  -> '0'; 1  -> '1'; 2  -> '2'; 3  -> '3'
                           4  -> '4'; 5  -> '5'; 6  -> '6'; 7  -> '7'
                           8  -> '8'; 9  -> '9'; 10 -> 'a'; 11 -> 'b'
                           12 -> 'c'; 13 -> 'd'; 14 -> 'e'; 15 -> 'f'
                           m  -> hex' (m `mod` 16)
          in  (hex' (w `div` 16), hex' (w `mod` 16))

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
