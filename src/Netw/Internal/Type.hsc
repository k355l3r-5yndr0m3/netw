{-# LANGUAGE ForeignFunctionInterface #-}
module Netw.Internal.Type 
( Iovec(..)
, sizeofSockaddrStorage
, Protoent(..), protoents
, Servent(..), servents
, _IPPORT_RESERVED, _IPPORT_USERRESERVED
) where
#include <netdb.h>
#include <sys/socket.h>

import Foreign
import Foreign.C

import Data.Primitive
import Data.Function

import Data.Word
import GHC.ByteOrder

data Iovec = Iovec { iovBase :: !(Ptr ()), iovLen :: !CSize }
instance Storable Iovec where
  sizeOf   _ = #{size struct iovec}
  alignment _ = #{alignment struct iovec}

  peek ptr = Iovec <$> #{peek struct iovec, iov_base} ptr <*> #{peek struct iovec, iov_len} ptr
  poke ptr iovec = #{poke struct iovec, iov_base} ptr (iovBase iovec)
                >> #{poke struct iovec, iov_len}  ptr (iovLen  iovec)

sizeofSockaddrStorage :: Int
sizeofSockaddrStorage = #{size struct sockaddr_storage}

data Protoent = Protoent
  { pName    :: String
  , pAliases :: [String]
  , pProto   :: CInt
  }

peekProtoent :: Ptr Protoent -> IO Protoent
peekProtoent ptr = do
  name    <- peekCString =<< #{peek struct protoent, p_name} ptr
  proto   <- #{peek struct protoent, p_proto} ptr
  aliases <- mapM peekCString =<< peekArray0 nullPtr =<< #{peek struct protoent, p_aliases} ptr
  return (Protoent name aliases proto)

foreign import ccall unsafe "getprotoent"
  getprotoent :: IO (Ptr Protoent)
foreign import ccall unsafe "endprotoent"
  endprotoent :: IO ()

protoents :: IO [Protoent]
protoents = do
  endprotoent
  fix (\ loop -> do
    proto' <- getprotoent
    if proto' == nullPtr
    then do
      endprotoent
      return []
    else do
      proto <- peekProtoent proto'
      (proto:) <$> loop
    )

data Servent = Servent
  { sName    :: String
  , sAliases :: [String]
  , sPort    :: Word16  -- NOTE: This is in host byte order
  , sProto   :: String
  }

peekServent :: Ptr Servent -> IO Servent
peekServent ptr = do
  name    <- peekCString =<< #{peek struct servent, s_name} ptr
  aliases <- mapM peekCString =<< peekArray0 nullPtr =<< #{peek struct servent, s_aliases} ptr
  port :: CInt
          <- #{peek struct servent, s_port} ptr
  proto   <- peekCString =<< #{peek struct servent, s_proto} ptr
  return (Servent name aliases (ntoh $ fromIntegral port) proto)
  where ntoh :: Word16 -> Word16
        ntoh = case targetByteOrder of LittleEndian -> byteSwap16; BigEndian -> id

foreign import ccall unsafe "getservent"
  getservent :: IO (Ptr Servent)
foreign import ccall unsafe "endservent"
  endservent :: IO ()

servents :: IO [Servent]
servents = do
  endservent
  fix (\ loop -> do
    serv' <- getservent
    if serv' == nullPtr
    then do
      endservent
      return []
    else do
      serv <- peekServent serv'
      (serv:) <$> loop
    )

_IPPORT_RESERVED :: Word16
_IPPORT_RESERVED = #{const IPPORT_RESERVED}

_IPPORT_USERRESERVED :: Word16
_IPPORT_USERRESERVED = #{const IPPORT_USERRESERVED}
