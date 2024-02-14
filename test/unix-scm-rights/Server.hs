module Server where
import Netw.Socket
import Netw.SockAddr
import Netw.Ancillary

import Control.Monad

import Data.Bits
import Data.List
import Data.Primitive

import System.Posix

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
        
