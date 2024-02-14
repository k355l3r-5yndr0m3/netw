{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Netw.SockAddr 
( SockAddr (..), Addr(..)
, mkAddr
, newSockAddrStorage
, SockAddrNull (..), SockAddrUn (..)
, SockAddrIn (..), SockAddrIn6 (..)
) where
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
import Netw.Internal.Type (sizeofSockaddrStorage)
import Netw.Inet

import Control.Exception (assert)
import Control.Monad.Primitive

import Data.Char
import Data.Functor
import Data.Foldable

import Data.Primitive
import Data.Primitive.ByteArray.Unaligned

import GHC.Word

type CSaFamily = #{type sa_family_t} -- Don't export this, it is only used here

-- | A type that represent some socket address
newtype Addr = Addr { unAddr :: forall a. SockAddr a => Maybe a }
mkAddr :: ByteArray -> Addr
mkAddr ba = Addr (byteArrayToSockAddr ba)

class SockAddr a where
  {-# MINIMAL sockAddrToByteArray, byteArrayToSockAddr #-}
  -- | Convert a socket address to its C equivalent
  sockAddrToByteArray :: a         -> ByteArray
  -- | Convert a socket address from its C equivalent
  byteArrayToSockAddr :: ByteArray -> Maybe a

  -- | Check if this bytearray contain a socket address of this type
  isByteArrayThisAddr :: ByteArray -> Bool
  isByteArrayThisAddr ba
    | Just _ <- byteArrayToSockAddr @a ba = True
    | otherwise = False

  -- | Find the required number of bytes to store the C struct
  sockAddrSize :: a -> Int
  sockAddrSize = sizeofByteArray . sockAddrToByteArray

-- TODO: Move this to an internal module
newSockAddrStorage :: PrimMonad m => m (MutableByteArray (PrimState m))
newSockAddrStorage = newByteArray sizeofSockaddrStorage

-- | A null socket address. Equivalent to not specifying a socket address.
data SockAddrNull = SockAddrNull
instance SockAddr SockAddrNull where
  isByteArrayThisAddr = (0 ==) . sizeofByteArray
  sockAddrSize = const 0

  sockAddrToByteArray _ = emptyByteArray
  byteArrayToSockAddr a
    | sizeofByteArray a == 0 = Just SockAddrNull
    | otherwise              = Nothing

-- | UNIX Socket address
newtype SockAddrUn = SockAddrUn FilePath deriving (Eq, Show)
-- NOTE: There is an extension on linux that add abstract socket address for
-- unix sockets. It is quite nonportable according to the man page
-- might implement it
instance SockAddr SockAddrUn where
  sockAddrSize (SockAddrUn sunPath) = #{offset struct sockaddr_un, sun_path} + length sunPath
  isByteArrayThisAddr = isSaFamily #{const AF_LOCAL}

  sockAddrToByteArray addr@(SockAddrUn sunPath) = runByteArray $ do
    sockaddr <- newByteArray len
    writeSaFamily sockaddr #{const AF_LOCAL}
    nullAt <- foldlM (\ idx char -> assert (isAscii char) (writeByteArray @Word8 sockaddr idx (toEnum (ord char))) >> return (succ idx)) #{offset struct sockaddr_un, sun_path} sunPath 
    writeByteArray @Word8 sockaddr nullAt 0
    return sockaddr
    where len = sockAddrSize addr
  byteArrayToSockAddr ba -- TODO: Check if the address is a linux abstract address (which start with a null byte)
    | isByteArrayThisAddr @SockAddrUn ba =
      let lstIdx  = sizeofByteArray ba - 1 {- NOTE on linux a NULL byte is appened to the end if the NULL byte is present -}
          addrStr = [let char = chr (fromEnum ch) in assert (isAscii char) char | ch <- [#{offset struct sockaddr_un, sun_path}..lstIdx] <&> indexByteArray @Word8 ba, ch /= 0]
      in  Just (SockAddrUn addrStr)
    | otherwise = Nothing

-- | IPv4 socket address
data SockAddrIn = SockAddrIn
  { sinPort :: !Port
  , sinAddr :: !InAddr
  }
instance SockAddr SockAddrIn where
  sockAddrSize _      = #{size struct sockaddr_in}
  isByteArrayThisAddr = isSaFamily #{const AF_INET}

  sockAddrToByteArray a@(SockAddrIn port addr) = runByteArray $ do
    sockaddr <- newByteArray len
    writeSaFamily sockaddr #{const AF_INET}
    writeAddr sockaddr portOffs (getPortInBE port)
    writeAddr sockaddr addrOffs addr
    return sockaddr
    where len = sockAddrSize a
          portOffs = #{offset struct sockaddr_in, sin_port}
          addrOffs = #{offset struct sockaddr_in, sin_addr} + #{offset struct in_addr, s_addr}
  byteArrayToSockAddr ba
    | isByteArrayThisAddr @SockAddrIn ba && sizeofByteArray ba >= #{size struct sockaddr_in} = -- #{offset struct sockaddr_in, sin_addr} + #{size struct in_addr} =
      let portOffs = #{offset struct sockaddr_in, sin_port}
          addrOffs = #{offset struct sockaddr_in, sin_addr} + #{offset struct in_addr, s_addr}
          addr = SockAddrIn { sinPort = portFromBE $ indexAddr ba portOffs
                            , sinAddr = indexAddr ba addrOffs
                            }
      in  Just addr
    | otherwise = Nothing

-- | IPv6 socket address
data SockAddrIn6 = SockAddrIn6
  { sin6Port     :: !Port
  , sin6Flowinfo :: !Word32
  , sin6Addr     :: !In6Addr
  , sin6ScopeId  :: !Word32
  }
instance SockAddr SockAddrIn6 where
  sockAddrSize _ = #{size struct sockaddr_in6}
  isByteArrayThisAddr = isSaFamily #{const AF_INET6}

  sockAddrToByteArray a@(SockAddrIn6 port flow (In6Addr## (## high##, low## ##)) scope) = runByteArray $ do
    sockaddr <- newByteArray len
    writeSaFamily sockaddr #{const AF_INET6}
    writeAddr sockaddr portOffs     (getPortInBE port)
    writeAddr sockaddr flowOffs     (hton32 flow)
    writeAddr sockaddr addrHighOffs (W64## high##)
    writeAddr sockaddr addrLowOffs  (W64## low##)
    writeAddr sockaddr scopeOffs    (hton32 scope)
    return sockaddr
    where len = sockAddrSize a
          portOffs  = #{offset struct sockaddr_in6, sin6_port}
          flowOffs  = #{offset struct sockaddr_in6, sin6_flowinfo}
          scopeOffs = #{offset struct sockaddr_in6, sin6_scope_id}
          addrOffs  = #{offset struct sockaddr_in6, sin6_addr} + #{offset struct in6_addr, s6_addr}
          addrHighOffs = addrOffs
          addrLowOffs  = addrOffs + sizeOfType @Word64

  byteArrayToSockAddr ba
    | isByteArrayThisAddr @SockAddrIn6 ba && sizeofByteArray ba >= #{size struct sockaddr_in6} =
      let portOffs  = #{offset struct sockaddr_in6, sin6_port}
          flowOffs  = #{offset struct sockaddr_in6, sin6_flowinfo}
          scopeOffs = #{offset struct sockaddr_in6, sin6_scope_id}
          addrOffs  = #{offset struct sockaddr_in6, sin6_addr} + #{offset struct in6_addr, s6_addr}
          addrHighOffs = addrOffs
          addrLowOffs  = addrOffs + sizeOfType @Word64
          !(W64## high##) = indexAddr ba addrHighOffs
          !(W64## low##)  = indexAddr ba addrLowOffs
          addr = SockAddrIn6 { sin6Port     = portFromBE $ indexAddr ba portOffs
                             , sin6Flowinfo = ntoh32 (indexAddr ba flowOffs)
                             , sin6Addr     = In6Addr## (## high##, low## ##)
                             , sin6ScopeId  = ntoh32 (indexAddr ba scopeOffs)
                             }
      in  Just addr
    | otherwise = Nothing

-- this is used internally
writeSaFamily :: PrimMonad m => MutableByteArray (PrimState m) -> CSaFamily -> m ()
writeSaFamily = (`writeByteArray` 0)

writeAddr :: (PrimUnaligned a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeAddr = writeUnalignedByteArray

indexAddr :: PrimUnaligned a => ByteArray -> Int -> a
indexAddr = indexUnalignedByteArray

isSaFamily :: CSaFamily -> ByteArray -> Bool
isSaFamily fam ba = sizeofByteArray ba >= #{size sa_family_t} && fam == indexByteArray @CSaFamily ba 0
