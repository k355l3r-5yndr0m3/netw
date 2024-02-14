{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
module Netw.Internal.Cmsg where
#include <sys/socket.h>

import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.ByteArray.Unaligned

import Foreign
import Foreign.C

deriving newtype instance PrimUnaligned CSize

type CmsghdrType  = CInt
type CmsghdrLevel = CInt

data {-# CTYPE "sys/socket.h" "struct cmsghdr" #-} Cmsghdr  
data {-# CTYPE "unsigned char" #-} Cmsgdata

-- these function are not too nessisary
foreign import capi unsafe "sys/socket.h CMSG_ALIGN"
  _CMSG_ALIGN :: CSize -> CSize
foreign import capi unsafe "sys/socket.h CMSG_SPACE"
  _CMSG_SPACE :: CSize -> CSize
foreign import capi unsafe "sys/socket.h CMSG_LEN"
  _CMSG_LEN   :: CSize -> CSize
foreign import capi unsafe "sys/socket.h CMSG_DATA"
  _CMSG_DATA  :: Ptr Cmsghdr -> Ptr Cmsgdata

foreign import ccall unsafe "cmsg_nxthdr"
  _CMSG_NXTHDR   :: Ptr a -> CSize -> Ptr Cmsghdr -> IO (Ptr Cmsghdr)
foreign import ccall unsafe "cmsg_firsthdr"
  _CMSG_FIRSTHDR :: Ptr a -> CSize -> Ptr Cmsghdr

cmsgSpace :: Int -> Int
cmsgSpace = fromIntegral . _CMSG_SPACE . fromIntegral

{- |
 - `peekCmsg buffer hdroffs reader`
 - `reader` is a function that accept the control message level and type, and the data payload.
 - The payload is given as a buffer, the offset of the payload section (cmsg_data), and the size of the payload (cmsg_len - CMSG_LEN(0)).
 - -}
peekCmsg :: ByteArray -> Int -> (CmsghdrLevel -> CmsghdrType -> ByteArray -> Int -> Int -> a) -> a
peekCmsg buffer hdroffs reader = reader cmsgLevel cmsgType buffer (hdroffs + cmsghdrDataOffset) (fromIntegral $ cmsgSize - cmsghdrSize)
  where cmsgSize  = indexUnalignedByteArray @CSize        buffer (hdroffs + #{offset struct cmsghdr, cmsg_len})
        cmsgLevel = indexUnalignedByteArray @CmsghdrLevel buffer (hdroffs + #{offset struct cmsghdr, cmsg_level})
        cmsgType  = indexUnalignedByteArray @CmsghdrType  buffer (hdroffs + #{offset struct cmsghdr, cmsg_type})
        cmsghdrDataOffset = #{const (int)(ptrdiff_t)((char*)CMSG_DATA((struct cmsghdr*)0x01) - (char*)0x01)}
        cmsghdrSize       = _CMSG_LEN 0

{- |
 - `pokeCmsg buffer hdroffs cmsgLevel cmsgType cmsgPayloadLen cmsgPayload`
 - Store a control message element info the `buffer` at `hdroffs`.
 - `cmsgPayloadLen` is the length of the data section.
 - `cmsgPayload` is a function that take a buffer, offset into that buffer, and write the data section at that location.
 - -}
pokeCmsg :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> CmsghdrLevel -> CmsghdrType -> Int -> (MutableByteArray (PrimState m) -> Int -> m ()) -> m ()
pokeCmsg buffer hdroffs cmsgLevel cmsgType cmsgPayloadLen cmsgPayload = do
  writeUnalignedByteArray @_ @CSize        buffer (hdroffs + #{offset struct cmsghdr, cmsg_len})   (_CMSG_LEN $ fromIntegral cmsgPayloadLen)
  writeUnalignedByteArray @_ @CmsghdrLevel buffer (hdroffs + #{offset struct cmsghdr, cmsg_level}) cmsgLevel
  writeUnalignedByteArray @_ @CmsghdrType  buffer (hdroffs + #{offset struct cmsghdr, cmsg_type})  cmsgType
  cmsgPayload buffer (hdroffs + cmsghdrDataOffset)
  where cmsghdrDataOffset = #{const (int)(ptrdiff_t)((char*)CMSG_DATA((struct cmsghdr*)0x01) - (char*)0x01)}

newCmsgBuffer :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newCmsgBuffer totalSize = do
  buffer <- newAlignedPinnedByteArray totalSize #{alignment struct cmsghdr}
  fillByteArray buffer 0 totalSize 0
  return buffer
