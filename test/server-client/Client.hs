{-# LANGUAGE OverloadedLists #-}
module Client where
import Netw.Socket
import Netw.SockAddr

import Control.Monad

import Data.Bits
import Data.Primitive


clientMain :: FilePath -> IO ()
clientMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect server addr
  void (sendall server [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] zeroBits)
  msg <- recvsome server 128 zeroBits 
  putStrLn ("CLIENT RECEIVED " ++ show (sizeofByteArray msg) ++ " BYTES")
  closeSocket server
  where addr = SockAddrUn sunPath
