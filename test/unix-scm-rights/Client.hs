{-# LANGUAGE OverloadedLists #-}
module Client where
import Netw.Socket
import Netw.SockAddr
import Netw.Ancillary

import Control.Monad

import Data.Bits
import Data.Primitive

import System.Posix
import System.Posix.IO


clientMain :: FilePath -> IO ()
clientMain sunPath = do
  server <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect server addr
  fd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
  void (sendmsg server SockAddrNull [([0], 0, 1)] [mkAncillaryData (ScmRights [fd])] zeroBits)
  closeSocket server
  where addr = SockAddrUn sunPath
