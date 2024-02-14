module Server where
import Netw.Socket
import Netw.SockAddr

import Control.Monad

import Data.Bits
import Data.Function
import Data.Primitive

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
        
