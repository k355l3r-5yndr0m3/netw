{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-|
Module      : Netw.Socket
Description : Core functions
Portabilty  : Linux

This module contains all the functions required to create and use sockets.
-}
module Netw.Socket 
( Socket
, socket, closeSocket
, bind, listen, getsockname
, accept, accept_
, connect

, sendall, send, send'
, sendallto, sendto, sendto'
, sendmsg, sendmsg'

, recvsome, recv, recv'
, recvfrom, recvfrom', recvsomefrom
, recvmsg, recvmsg_

, shutdown
, module Netw.Internal.Enum
) where
import qualified Netw.Internal.Socket as I
import Netw.Internal.Enum
import Netw.Internal.Type
import Netw.Internal.Cmsg (newCmsgBuffer)

import Netw.SockAddr
import Netw.Ancillary

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import Data.Function
import Data.Functor
import Data.Primitive
import Data.Word

import Foreign
import Foreign.C.Types

import System.Posix.Types
import System.Posix.IO

import GHC.Exts

-- |
-- A file descriptor that is a socket.
-- All socket used by this library is in nonblocking mode (O_NONBLOCK)
newtype Socket = MkSocket { unSocket :: Fd } 

-- | Create a new socket (see `man 3 socket`)
socket :: ProtocolFamily -> SocketType -> Protocol -> IO Socket
socket domain stype protocol = MkSocket <$> I.socket domain stype protocol

-- | Close a socket (see `man 3 close`)
closeSocket :: Socket -> IO ()
closeSocket = closeFd . unSocket

-- | Bind a socket to an address (see `man 3 bind`)
bind :: SockAddr a => Socket -> a -> IO ()
bind (unSocket -> sock) sockaddr@(sockAddrToByteArray -> addr@(ByteArray addr#))
  | sockAddrSize sockaddr > 0 = I.bind# sock addr# (fromIntegral $ sizeofByteArray addr)
  | otherwise                 = I.bind  sock nullPtr 0

-- | Mark a socket as accepting connection (see `man 3 listen`)
listen :: Socket -> Int -> IO ()
listen (unSocket -> sock) (fromIntegral -> backlog) =
  I.listen sock backlog

-- | Retrieve the name of the socket (see `man 3 getsockname`)
getsockname :: Socket -> IO ByteArray
getsockname (unSocket -> sock) = 
  with 0 $ \ sockaddrLen -> do
    I.getsockname sock nullPtr sockaddrLen
    sockaddr@(MutableByteArray sockaddr#) <- newByteArray . fromIntegral =<< peek sockaddrLen
    I.getsockname# sock sockaddr# sockaddrLen
    unsafeFreezeByteArray sockaddr

-- | Extract the first connection on the pending connection queue (see `man 3 accept`). This function blocks the calling thread until a connection is made.
accept :: Socket -> IO (Socket, ByteArray)
accept (unSocket -> sock) = 
  with sockaddrLenGuess $ \ sockaddrLen -> do -- TODO: Check the doc if the address returned from accept is the same as getsockname
    sockaddr@(MutableByteArray sockaddr#) <- newByteArray (fromIntegral sockaddrLenGuess)
    peer <- MkSocket <$> I.accept# sock sockaddr# sockaddrLen
    len  <- peek sockaddrLen
    (peer, ) <$>
      if len > sockaddrLenGuess
      then do
        addr@(MutableByteArray addr#) <- newByteArray (fromIntegral len)
        I.getsockname# (unSocket peer) addr# sockaddrLen
        unsafeFreezeByteArray addr
      else do
        unsafeFreezeByteArray =<< resizeMutableByteArray sockaddr (fromIntegral len)
  where sockaddrLenGuess = 128

-- | A version of `accept` which does not return the peer address
accept_ :: Socket -> IO Socket
accept_ (unSocket -> sock) = MkSocket <$> I.accept sock nullPtr nullPtr

-- | Try to connect to an address (see `man 3 connect`)
connect :: SockAddr a => Socket -> a -> IO ()
connect (unSocket -> sock) sockaddr@(sockAddrToByteArray -> addr@(ByteArray addr#))
  | sockAddrSize sockaddr > 0 = I.connect# sock addr# (fromIntegral $ sizeofByteArray addr)
  | otherwise                 = I.connect  sock nullPtr 0

-- You can shave off maybe one clock cycle if this is change to use send and not send_wrapper


-- |
-- `send socket buffer offset size flags`
-- Send a range of bytes in buffer located at offset and is size bytes long.
-- Return the number of bytes accually sent which may be less than
-- the number of bytes specified.
-- Block the current thread until some bytes are sent.
-- (see `man 3 send`)
send :: Socket -> ByteArray -> Int -> Int -> MsgFlags -> IO Int
send (unSocket -> fd) (ByteArray buf#) offs size flags = fromIntegral <$> I.send# fd buf# (fromIntegral offs) (fromIntegral size) flags

-- | A version of `send` that accept a pointer instead of a bytearray.
send' :: Socket -> Ptr () -> Int -> MsgFlags -> IO Int
send' (unSocket -> sockfd) buf len flags = fromIntegral <$> I.send sockfd buf (fromIntegral len) flags

-- | 
-- Send the whole content of a bytearray through a socket.
-- Block until the entire bytearray has been sent.
sendall :: Socket -> ByteArray -> MsgFlags -> IO ()
sendall sock buf flags = fix (\ resend offs size ->
  when (size > 0) $ do
    sent <- send sock buf offs size flags
    resend (offs + sent) (size - sent)) 0 (sizeofByteArray buf)

-- |
-- A version of `send` that accept the destination address (see `man 3 sendto`)
-- Return the number of bytes accually sent which may be less than
-- the number of bytes specified.
-- Block the thread until some bytes are sent.
sendto :: SockAddr a => Socket -> ByteArray -> Int -> Int -> MsgFlags -> a -> IO Int
sendto (unSocket -> fd) (ByteArray buf#) offs size flags (sockAddrToByteArray -> addr@(ByteArray addr#)) =
  fromIntegral <$> I.sendto# fd buf# (fromIntegral offs) (fromIntegral size) flags addr# (fromIntegral $ sizeofByteArray addr)

-- | A version of `sendto` that accept a pointer instead of a bytearray
sendto' :: SockAddr a => Socket -> Ptr () -> Int -> MsgFlags -> a -> IO Int
sendto' (unSocket -> sockfd) buf (fromIntegral -> len) flags (sockAddrToByteArray -> addr@(ByteArray addr#)) =
  fromIntegral <$> I.sendto sockfd buf len flags addr# (fromIntegral $ sizeofByteArray addr)

-- | Send the whole content of a bytearray
sendallto :: SockAddr a => Socket -> ByteArray -> MsgFlags -> a -> IO ()
sendallto sock buf flag addr = 
  fix (\ resend offs size -> 
    when (size > 0) $ do
      sent <- sendto sock buf offs size flag addr 
      resend (offs + sent) (size - sent)) 0 (sizeofByteArray buf)

-- lifting (TODO: Find a better more anonymous method)
data State s = State (State# s)
data ArrayByteArray = ArrayByteArray (Array# ByteArray#)
data ArrayMutableByteArray = ArrayMutableByteArray (Array# (MutableByteArray# RealWorld))

{-|
`sendmsg socket address iovecs cmsgs flags`
The iovecs argument is a list of 3-tuples of the data buffer, the offset in bytes,
and the number of bytes to send. Use SockAddrNull if no address is to be specified.
This function blocks the calling thread and return the number of bytes sent, which
may be less than the intended message size.

(See `man 3 sendmsg`)
-}
sendmsg :: SockAddr a => Socket -> a -> [(ByteArray, Int, Int)] -> [AncillaryData] -> MsgFlags -> IO Int
sendmsg (unSocket -> sockfd) addr (unzip3 -> (iovecs, offs, size)) (encodeAncillaryData -> control) flags = 
  withByteArrayContents control $ \ control' ->
    fromIntegral <$> I.sendmsg# sockfd 
                                addr# (fromIntegral addrLen)
                                iovecs# offs# size# (fromIntegral iovlen)
                                (castPtr control') (fromIntegral $ sizeofByteArray control)
                                flags
  where !(PrimArray offs#) = fromList (fromIntegral <$> offs) :: PrimArray CPtrdiff
        !(PrimArray size#) = fromList (fromIntegral <$> size) :: PrimArray CSize
        !(ArrayByteArray iovecs#) = runST $ primitive $ \ state0# ->
          let !(# state1#, mutarr# #) 
                = newArray# iovlen# emptyByteArray# state0# in
          let !(State state4#, _)
                = foldl (\ (State state2#, idx@(I# idx#)) (ByteArray iov#) ->
                    let state3# = writeArray# mutarr# idx# iov# state2#
                    in  (State state3#, idx + 1) ) (State state1#, 0 :: Int) iovecs in
          let !(# state5#, arr# #)
                = unsafeFreezeArray# mutarr# state4# in
          (# state5#, ArrayByteArray arr# #)
        !iovlen@(I# iovlen#) = length iovecs
        !(ByteArray emptyByteArray#) = emptyByteArray
        !(ByteArray addr#) = sockAddrToByteArray addr
        addrLen = sockAddrSize addr

-- | A version of `sendmsg` that uses pointers instead of bytearrays
sendmsg' :: SockAddr a => Socket -> a -> [(Ptr Word8, Int)] -> [AncillaryData] -> MsgFlags -> IO Int
sendmsg' (unSocket -> sockfd) addr iovecs (encodeAncillaryData -> control) flags =
  withByteArrayContents control $ \ control' ->
  withArray iov $ \ iov' ->
    fromIntegral <$> I.sendmsg sockfd
                               addr# (fromIntegral addrLen)
                               iov' (fromIntegral iovlen)
                               (castPtr control') (fromIntegral $ sizeofByteArray control)
                               flags
  where addrLen = sockAddrSize addr
        !(ByteArray addr#) = sockAddrToByteArray addr
        iov = iovecs <&> \ (ptr, len) -> Iovec (castPtr ptr) (fromIntegral len)
        iovlen = length iovecs

{-|
`recvsome socket n flags`
Receive a maximum of `n` bytes from `socket`.
Return the message as a bytearray. If the bytearray is empty (size == 0) then there is no more data to be read and the socket
should be closed.
-}
recvsome :: Socket -> Int -> MsgFlags -> IO ByteArray
recvsome sock maxBytesRecv flags = do
  mutarr    <- newByteArray maxBytesRecv
  bytesRecv <- recv sock mutarr 0 maxBytesRecv flags
  shrinkMutableByteArray mutarr (fromIntegral bytesRecv)
  unsafeFreezeByteArray mutarr

{-|
`recv socket buffer offset size flags`
Receive a maximum of `size` bytes from `socket`, storing those bytes in `buffer` at `offset`
Return the number of bytes received. (See `man 3 recv`)
Block the calling thread until some bytes are received.
-}
recv :: Socket -> MutableByteArray RealWorld -> Int -> Int -> MsgFlags -> IO Int
recv (unSocket -> sockfd) (MutableByteArray buf#) offs size = fmap fromIntegral . I.recv# sockfd buf# (fromIntegral offs) (fromIntegral size)

-- | A version of `recv` that takes a pointer instead of a mutablebytearray
recv' :: Socket -> Ptr () -> Int -> MsgFlags -> IO Int 
recv' (unSocket -> sockfd) buf size = fmap fromIntegral . I.recv sockfd buf (fromIntegral size)

recvfromImpl :: Socket -> MutableByteArray RealWorld -> Int -> Int -> MsgFlags -> IO (Int, ByteArray)
recvfromImpl (unSocket -> sockfd) (MutableByteArray buf#) offs size flags = do
  addr@(MutableByteArray addr#) <- newSockAddrStorage
  addrlen <- getSizeofMutableByteArray addr
  with (fromIntegral addrlen) $ \ addrlen' -> do
    bytesRecv <-
      I.recvfrom# sockfd
                  buf# (fromIntegral offs) (fromIntegral size)
                  flags addr# addrlen'
    shrinkMutableByteArray addr . fromIntegral =<< peek addrlen'
    (fromIntegral bytesRecv,) <$> unsafeFreezeByteArray addr 

{-|
`recvfrom socket buffer offset size flags`
Receive a message less than `size` bytes long and the source address.
The bytes received are stored in `buffer` at `offset`
Return the number of bytes received.

(See `man 3 recvfrom`)
-}
recvfrom :: Socket -> MutableByteArray RealWorld -> Int -> Int -> MsgFlags -> IO (Int, Addr)
recvfrom sock buf offs size flags = fmap mkAddr <$> recvfromImpl sock buf offs size flags

-- | A version of `recvfrom` that takes a pointer
recvfrom' :: Socket -> Ptr () -> Int -> MsgFlags -> IO (Int, Addr)
recvfrom' (unSocket -> sockfd) buf len flags = fmap mkAddr <$> do
  addr@(MutableByteArray addr#) <- newSockAddrStorage
  addrlen <- getSizeofMutableByteArray addr
  with (fromIntegral addrlen) $ \ addrlen' -> do
    bytesRecv <-
      I.recvfrom sockfd
                 buf (fromIntegral len)
                 flags addr# addrlen'
    shrinkMutableByteArray addr . fromIntegral =<< peek addrlen'
    (fromIntegral bytesRecv,) <$> unsafeFreezeByteArray addr 

-- | A version of recvfrom that automatically allocate a bytearray
recvsomefrom :: Socket -> Int -> MsgFlags -> IO (ByteArray, Addr)
recvsomefrom sock size flags = do
  buf <- newByteArray size
  (bytesRecv, addr) <- recvfromImpl sock buf 0 size flags
  shrinkMutableByteArray buf (fromIntegral bytesRecv)
  (, mkAddr addr) <$> unsafeFreezeByteArray buf

{- |
Receive messages alongside ancillary data.
The elements of `iovecs` are tuples containing the storage bytearray, offset into bytearray, and
the number of bytes to write to said bytearray.

(See `man 3 recvmsg`)
-}
recvmsg :: Socket -> [(MutableByteArray RealWorld, Int, Int)] -> Int -> MsgFlags -> IO (Int, MsgFlags, [AncillaryData], Addr)
recvmsg (unSocket -> sockfd) (unzip3 -> (iovecs, map fromIntegral -> offs :: [CPtrdiff], map fromIntegral -> size :: [CSize])) controllen flags = do
  control <- newCmsgBuffer controllen
  name@(MutableByteArray sockaddr#) 
          <- newSockAddrStorage
  namelen <- getSizeofMutableByteArray name
  (ArrayMutableByteArray iov#) <- primitive $ \ state0# -> 
    let !(# state1#, dummy# #) 
          = newByteArray# _0# state0# in
    let !(# state2#, mutarr# #)
          = newArray# iovlen# dummy# state1# in
    let !(State state4#, _)
          = foldl (\ (State state3#, idx@(I# idx#)) (MutableByteArray ba#) -> (State (writeArray# mutarr# idx# ba# state3#), idx + 1))
                  (State state2#, 0)
                  iovecs in
    let !(# state5#, arr# #)
          = unsafeFreezeArray# mutarr# state4# in
    (# state5#, ArrayMutableByteArray arr# #)
  (bytesRecv, newFlags, newControllen, newNamelen) <-
    withMutableByteArrayContents control $ \ control' ->
    with (fromIntegral controllen) $ \ controllen' ->
    with (fromIntegral namelen) $ \ namelen' ->
    with flags $ \ flags' -> do
      bytesRecv <- fromIntegral <$> 
        I.recvmsg# sockfd
                   sockaddr# namelen'
                   iov# offs# size# (fromIntegral iovlen)
                   control' controllen'
                   flags'
      newFlags   <- peek flags'
      newControllen
                 <- peek controllen'
      newNamelen <- peek namelen'
      return (bytesRecv, newFlags, newControllen, newNamelen)
  shrinkMutableByteArray name (fromIntegral newNamelen)
  shrinkMutableByteArray control (fromIntegral newControllen)

  frozenName    <- unsafeFreezeByteArray name
  frozenControl <- unsafeFreezeByteArray control
  return (bytesRecv, newFlags, decodeAncillaryData frozenControl, mkAddr frozenName)
  where !iovlen@(I# iovlen#) = length iovecs
        !(I# _0#) = 0
        !(PrimArray offs#) = fromList offs
        !(PrimArray size#) = fromList size

-- | A version of `recvmsg` that does not return the address
recvmsg_ :: Socket -> [(MutableByteArray RealWorld, Int, Int)] -> Int -> MsgFlags -> IO (Int, MsgFlags, [AncillaryData])
recvmsg_ (unSocket -> sockfd) (unzip3 -> (iovecs, map fromIntegral -> offs :: [CPtrdiff], map fromIntegral -> size :: [CSize])) controllen flags = do
  control <- newCmsgBuffer controllen
  (MutableByteArray sockaddr#) 
          <- newByteArray 0
  let namelen = 0 
  (ArrayMutableByteArray iov#) <- primitive $ \ state0# -> 
    let !(# state1#, dummy# #) 
          = newByteArray# _0# state0# in
    let !(# state2#, mutarr# #)
          = newArray# iovlen# dummy# state1# in
    let !(State state4#, _)
          = foldl (\ (State state3#, idx@(I# idx#)) (MutableByteArray ba#) -> (State (writeArray# mutarr# idx# ba# state3#), idx + 1))
                  (State state2#, 0)
                  iovecs in
    let !(# state5#, arr# #)
          = unsafeFreezeArray# mutarr# state4# in
    (# state5#, ArrayMutableByteArray arr# #)
  (bytesRecv, newFlags, newControllen) <-
    withMutableByteArrayContents control $ \ control' ->
    with (fromIntegral controllen) $ \ controllen' ->
    with namelen $ \ namelen' ->
    with flags $ \ flags' -> do
      bytesRecv <- fromIntegral <$> 
        I.recvmsg# sockfd
                   sockaddr# namelen'
                   iov# offs# size# (fromIntegral iovlen)
                   control' controllen'
                   flags'
      newFlags   <- peek flags'
      newControllen
                 <- peek controllen'
      return (bytesRecv, newFlags, newControllen)
  shrinkMutableByteArray control (fromIntegral newControllen)
  frozenControl <- unsafeFreezeByteArray control
  return (bytesRecv, newFlags, decodeAncillaryData frozenControl)
  where !iovlen@(I# iovlen#) = length iovecs
        !(I# _0#) = 0
        !(PrimArray offs#) = fromList offs
        !(PrimArray size#) = fromList size

-- | Shutdown all or part of of a duplex connection
shutdown :: Socket -> ShutHow -> IO ()
shutdown = I.shutdown . unSocket
