{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Netw.Socket
import Netw.SockAddr 

import Control.Monad

import Data.Function
import Data.Primitive
import Data.Bits
import Data.Word

import System.Posix
import Control.Concurrent (threadDelay)


messages :: [ByteArray]
messages = map mkByteArray [45, 742, 33, 554, 123, 74]
  where mkByteArray len = byteArrayFromListN @Word8 len (take len (cycle [0..]))
--messages =
--  [[0x03, 0xf1, 0x44, 0x11]
--  ,[0x5f, 0xc1, 0x02, 0xab]
--  ,[0x1c, 0xa2, 0xbf, 0x7d]
--  ,[0x33, 0x7d, 0x30, 0x0a]]

serverPath :: FilePath
serverPath = "/run/user/1000/unix-sock"

server :: IO ()
server = do
  servSock <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  bind servSock (SockAddrUn serverPath)
  listen servSock 1
  clieSock <- accept_ servSock
  
  bufs <- mapM (\ (sizeofByteArray -> n) -> (, 0, n) <$> newByteArray n) messages
  fix (\ recvmore buffers nbytes ->
    let incrementBuffer 0 b      = b
        incrementBuffer n (b:bs) =
          let (mba, offs, size) = b
          in  if size > n
              then
                (mba, offs + n, size - n):bs
              else
                incrementBuffer (n - size) bs
        incrementBuffer n []
          | n == 0    = []
          | otherwise = error "Impossible"
    in  when (nbytes > 0) $
      do
      (brecv, _, _) <- recvmsg_ clieSock buffers 0 zeroBits
      recvmore (incrementBuffer brecv buffers) (nbytes - brecv)) bufs (sum (sizeofByteArray <$> messages))
  receivedMessage <- mapM (\ (b, _, _) -> unsafeFreezeByteArray b) bufs
  
  print (if receivedMessage == messages then "Success" else "Failure")

  closeSocket clieSock
  closeSocket servSock
  removeLink serverPath

client :: IO ()
client = do
  servSock <- socket PF_UNIX SOCK_STREAM DefaultProtocol
  connect servSock (SockAddrUn serverPath)
  fix (\ sendmore buffers nbytes ->
    let incrementBuffer 0 b      = b
        incrementBuffer n (b:bs) =
          let (mba, offs, size) = b
          in  if size > n
              then
                (mba, offs + n, size - n):bs
              else
                incrementBuffer (n - size) bs
        incrementBuffer n []
          | n == 0    = []
          | otherwise = error "Impossible"
    in  when (nbytes > 0) $
      do
        bsent <- sendmsg servSock SockAddrNull buffers [] zeroBits
        sendmore (incrementBuffer bsent buffers) (nbytes - bsent)) (map (\ b -> (b, 0, sizeofByteArray b)) messages) (sum (map sizeofByteArray messages))
  closeSocket servSock

main :: IO ()
main = do
  _ <- forkProcess server
  _ <- forkProcess client
  threadDelay 50000
