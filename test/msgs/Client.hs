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
  
  void (sendmsg server SockAddrNull [(msg, 0, sizeofByteArray msg)] [] zeroBits)

  closeSocket server
  where addr = SockAddrUn sunPath
        msg  = [0, 1, 2, 3, 4, 5, 6, 7]
