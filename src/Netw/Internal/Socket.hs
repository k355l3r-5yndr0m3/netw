{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
module Netw.Internal.Socket 
( retryOrThrowErrnoIfMinus1
, throwErrnoIfNothing

, socket

, bind#
, bind

, listen

, getsockname#
, getsockname

, accept#
, accept

, connect#
, connect

, send#, send, sendto#
, sendto, sendmsg#, sendmsg

, recv#, recv
, recvfrom#, recvfrom
, recvmsg#, recvmsg

, shutdown
) where
import Netw.Internal.Enum
import Netw.Internal.Type

import Foreign
import Foreign.C

import System.Posix.Types

import GHC.Exts

retryOrThrowErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
retryOrThrowErrnoIfMinus1 msg action = do
  r <- action
  if r == -1
  then do
    errno <- getErrno
    if errno == eAGAIN || errno == eWOULDBLOCK then
      retryOrThrowErrnoIfMinus1 msg action
    else
      throwErrno msg
  else
    return r
-- Remember to check a (the returned value) to determine if an error occure
-- since something else might have failed before that and errno is no longer EOK
throwErrnoIfNothing :: String -> (a -> Errno -> Maybe b) -> IO a -> IO b
throwErrnoIfNothing msg func act = do
  ret <- act
  err <- getErrno
  let may = func ret err
  case may of
    Nothing -> throwErrno msg
    Just b  -> return b

zeroIfMinus1AndEAGAINOrEWOULDBLOCK :: (Num a, Eq a) => a -> Errno -> Maybe a
zeroIfMinus1AndEAGAINOrEWOULDBLOCK (-1) errno
  | errno == eAGAIN || errno == eWOULDBLOCK = Just 0
  | otherwise = Nothing
zeroIfMinus1AndEAGAINOrEWOULDBLOCK a _ = Just a

throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK :: (Num a, Eq a) => String -> IO a -> IO a
throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK msg act = do
  ret <- act
  if ret == -1 
  then do
    errno <- getErrno
    if errno == eAGAIN || errno == eWOULDBLOCK
    then 
      return 0
    else
      throwErrno msg
  else
    return ret

foreign import ccall unsafe "socket_wrapper"
  socket_  :: ProtocolFamily -> SocketType -> Protocol -> IO CInt
socket :: ProtocolFamily -> SocketType -> Protocol -> IO Fd
socket domain stype protocol = Fd <$>
  throwErrnoIfMinus1 "socket failed to create socket"
    (socket_ domain stype protocol)

foreign import ccall unsafe "bind"
  bind_#   :: Fd -> ByteArray# -> CSocklen -> IO CInt
bind# :: Fd -> ByteArray# -> CSocklen -> IO ()
bind# sockfd address addressLen =
  throwErrnoIfMinus1_ "bind failed to bind sockfd"
    (bind_# sockfd address addressLen)
foreign import ccall unsafe "bind"
  bind_    :: Fd -> Ptr () -> CSocklen -> IO CInt
bind :: Fd -> Ptr () -> CSocklen -> IO ()
bind sockfd address addressLen =
  throwErrnoIfMinus1_ "bind failed to bind sockfd"
    (bind_ sockfd address addressLen)


foreign import ccall unsafe "listen"
  listen_   :: Fd -> CInt -> IO CInt
listen :: Fd -> CInt -> IO ()
listen sockfd backlog =
  throwErrnoIfMinus1_ "listen fail"
    (listen_ sockfd backlog)

-- TODO: Add an extra argument to specify retry wait time
foreign import ccall unsafe "accept"
  accept_# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CInt
accept# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO Fd
accept# sockfd address addressLen = Fd <$>
  retryOrThrowErrnoIfMinus1 "accept failed" (accept_# sockfd address addressLen)
foreign import ccall unsafe "accept"
  accept_  :: Fd -> Ptr () -> Ptr CSocklen -> IO CInt    
accept :: Fd -> Ptr () -> Ptr CSocklen -> IO Fd
accept sockfd address addressLen = Fd <$>
  retryOrThrowErrnoIfMinus1 "accept failed" (accept_ sockfd address addressLen)

foreign import ccall unsafe "getsockname"
  getsockname_# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CInt
getsockname# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO ()
getsockname# sockfd address addressLen =
  throwErrnoIfMinus1_ "getsockname failed"
    (getsockname_# sockfd address addressLen)

foreign import ccall unsafe "getsockname"
  getsockname_ :: Fd -> Ptr () -> Ptr CSocklen -> IO CInt
getsockname :: Fd -> Ptr () -> Ptr CSocklen -> IO ()
getsockname sockfd address addressLen =
  throwErrnoIfMinus1_ "getsockname failed"
    (getsockname_ sockfd address addressLen)

foreign import ccall unsafe "connect"
  connect_# :: Fd -> ByteArray# -> CSocklen -> IO CInt
connect# :: Fd -> ByteArray# -> CSocklen -> IO ()
connect# sockfd address addressLen =
  throwErrnoIfMinus1_ "connect failed"
    (connect_# sockfd address addressLen)
foreign import ccall unsafe "connect"
  connect_  :: Fd -> Ptr () -> CSocklen -> IO CInt
connect :: Fd -> Ptr () -> CSocklen -> IO ()
connect sockfd address addressLen =
  throwErrnoIfMinus1_ "connect failed"
    (connect_ sockfd address addressLen)

-- Actually sending and recving data
-- The send* function might return zero in case no byte is sent
-- failure is handled
foreign import ccall unsafe "send_wrapper"
  send_# :: Fd -> ByteArray# -> CPtrdiff -> CSize -> MsgFlags -> IO CSsize 
send# :: Fd -> ByteArray# -> CPtrdiff -> CSize -> MsgFlags -> IO CSsize
send# fd buf# offs size flags =
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "send failed"
    (send_# fd buf# offs size flags)

foreign import ccall unsafe "send"
  send_ :: Fd -> Ptr () -> CSize -> MsgFlags -> IO CSsize 
send :: Fd -> Ptr () -> CSize -> MsgFlags -> IO CSsize
send fd buf size flags = 
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "send failed"
    (send_ fd buf size flags)

foreign import ccall unsafe "sendto_wrapper"
  sendto_# :: Fd -> ByteArray# -> CPtrdiff -> CSize -> MsgFlags -> ByteArray# -> CSocklen -> IO CSsize
sendto# :: Fd -> ByteArray# -> CPtrdiff -> CSize -> MsgFlags -> ByteArray# -> CSocklen -> IO CSsize
sendto# fd buf# offs size flags addr# addrlen =
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "sendto failed"
    (sendto_# fd buf# offs size flags addr# addrlen)
foreign import ccall unsafe "sendto"
  sendto_  :: Fd -> Ptr () -> CSize -> MsgFlags -> ByteArray# -> CSocklen -> IO CSsize
sendto :: Fd -> Ptr () -> CSize -> MsgFlags -> ByteArray# -> CSocklen -> IO CSsize
sendto fd buf size flags addr# addrlen = 
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "sendto failed"
    (sendto_ fd buf size flags addr# addrlen)

foreign import ccall unsafe "sendmsg_wrapper0" -- iovecs             offs         size          nvec
  sendmsg_# :: Fd -> ByteArray# -> CSocklen -> Array# ByteArray# -> ByteArray# -> ByteArray# -> CSize -> Ptr () -> CSize -> MsgFlags -> IO CSsize
sendmsg# :: Fd -> ByteArray# -> CSocklen -> Array# ByteArray# -> ByteArray# -> ByteArray# -> CSize -> Ptr () -> CSize -> MsgFlags -> IO CSsize
sendmsg# fd addr# addrlen iovecs offs size nvec cmsg cmsglen flags =
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "sendmsg failed"
    (sendmsg_# fd addr# addrlen iovecs offs size nvec cmsg cmsglen flags)

foreign import ccall unsafe "sendmsg_wrapper1"
  sendmsg_  :: Fd -> ByteArray# -> CSocklen -> Ptr Iovec -> CSize -> Ptr () -> CSize -> MsgFlags -> IO CSsize
sendmsg :: Fd -> ByteArray# -> CSocklen -> Ptr Iovec -> CSize -> Ptr () -> CSize -> MsgFlags -> IO CSsize
sendmsg fd addr# addrlen iovecs ioveclen cmsg cmsglen flags =
  throwErrnoIfMinus1UnlessEAGAINorEWOULDBLOCK "sendmsg failed"
    (sendmsg_ fd addr# addrlen iovecs ioveclen cmsg cmsglen flags)

foreign import ccall unsafe "recv_wrapper"
  recv_# :: Fd -> MutableByteArray# RealWorld -> CPtrdiff -> CSize -> MsgFlags -> IO CSsize
recv# :: Fd -> MutableByteArray# RealWorld -> CPtrdiff -> CSize -> MsgFlags -> IO CSsize
recv# fd buf# offs size flags =
  retryOrThrowErrnoIfMinus1 "recv failed"
    (recv_# fd buf# offs size flags)

foreign import ccall unsafe "recv"
  recv_ :: Fd -> Ptr () -> CSize -> MsgFlags -> IO CSsize
recv :: Fd -> Ptr () -> CSize -> MsgFlags -> IO CSsize
recv fd buf size flags =
  retryOrThrowErrnoIfMinus1 "recv failed"
    (recv_ fd buf size flags)

foreign import ccall unsafe "recvfrom_wrapper"
  recvfrom_# :: Fd -> MutableByteArray# RealWorld -> CPtrdiff -> CSize -> MsgFlags -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CSsize
recvfrom# :: Fd -> MutableByteArray# RealWorld -> CPtrdiff -> CSize -> MsgFlags -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CSsize
recvfrom# fd buf# offs size flags addr# addrlen =
  retryOrThrowErrnoIfMinus1 "recvfrom failed"
    (recvfrom_# fd buf# offs size flags addr# addrlen)

foreign import ccall unsafe "recvfrom"
  recvfrom_ :: Fd -> Ptr () -> CSize -> MsgFlags -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CSsize
recvfrom :: Fd -> Ptr () -> CSize -> MsgFlags -> MutableByteArray# RealWorld -> Ptr CSocklen -> IO CSsize
recvfrom fd buf size flags addr# addrlen =
  retryOrThrowErrnoIfMinus1 "recvfrom failed"
    (recvfrom_ fd buf size flags addr# addrlen)

foreign import ccall unsafe "recvmsg_wrapper0"
  recvmsg_# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> Array# (MutableByteArray# RealWorld) -> ByteArray# -> ByteArray# -> CSize -> Ptr a -> Ptr CSize -> Ptr MsgFlags -> IO CSsize
recvmsg# :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> Array# (MutableByteArray# RealWorld) -> ByteArray# -> ByteArray# -> CSize -> Ptr a -> Ptr CSize -> Ptr MsgFlags -> IO CSsize
recvmsg# fd addr# addrlen iovecs# offs size ioveclen cmsg cmsglen flags =
  retryOrThrowErrnoIfMinus1 "recvmsg failed"
    (recvmsg_# fd addr# addrlen iovecs# offs size ioveclen cmsg cmsglen flags)

foreign import ccall unsafe "recvmsg_wrapper1"
  recvmsg_  :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> Ptr Iovec -> CSize -> Ptr a -> Ptr CSize -> Ptr MsgFlags -> IO CSsize
recvmsg :: Fd -> MutableByteArray# RealWorld -> Ptr CSocklen -> Ptr Iovec -> CSize -> Ptr a -> Ptr CSize -> Ptr MsgFlags -> IO CSsize
recvmsg fd addr# addrlen iovecs ioveclen cmsg cmsglen flags =
  retryOrThrowErrnoIfMinus1 "recvmsg failed"
    (recvmsg_ fd addr# addrlen iovecs ioveclen cmsg cmsglen flags)

foreign import ccall unsafe "shutdown"
  shutdown_ :: Fd -> ShutHow -> IO CInt
shutdown :: Fd -> ShutHow -> IO ()
shutdown fd = throwErrnoIfMinus1_ "shutdown failed" . shutdown_ fd
