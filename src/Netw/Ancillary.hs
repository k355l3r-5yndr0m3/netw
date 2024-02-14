{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-|
Module      : Netw.Ancillary
Description : Ancillary data
Portabilty  : Linux

This module contains ancillary data types. Ancillary data can be send and received via the sendmsg and recvmsg functions.
-}
module Netw.Ancillary where
import Netw.Internal.Ancillary
import qualified Netw.Internal.Cmsg as I

import Control.Monad.Primitive
import Control.Monad
import Control.Exception

import Data.Dynamic
import Data.Foldable
import Data.Function
import Data.Primitive hiding (indexByteArray, writeByteArray, readByteArray)
import Data.Primitive.ByteArray.Unaligned

import System.Posix.Types

import GHC.TypeLits

class (Typeable a, KnownNatPair (Anci a)) => Ancillary a where
  -- | This type family enforces an unique mapping between cmsg_level and cmsg_type pair and the ancillary data type
  type Anci a = (r :: (Nat, Nat)) | r -> a
  -- ^ the order is (cmsg_level, cmsg_type)

  -- | Get the size of the cmsg payload (just cmsg_data[])
  cmsgDataSize :: a -> Int

  -- | Write the payload onto the data section. Take the offset of the data section.
  cmsgPokeData :: PrimMonad m => a -> MutableByteArray (PrimState m) -> Int -> m ()
  -- ^ This function assumes enough space has been allocated

  -- | Read the payload from the data section. Take the offset and size of the payload section.
  cmsgPeekData :: ByteArray -> Int -> Int -> a

cmsgLevel :: forall a. Ancillary a => I.CmsghdrLevel
cmsgLevel = fromIntegral (fstNatVal @(Anci a))

cmsgType :: forall a. Ancillary a => I.CmsghdrType
cmsgType = fromIntegral (sndNatVal @(Anci a))

-- | Find the space a control message element occupies in cmsg buffer given the size of its payload section.
cmsgSpace :: Ancillary a => a -> Int
cmsgSpace = I.cmsgSpace . cmsgDataSize

-- | Read a control message element, assuming type a. Return Nothing if a is not the correct type.
peekAncillary :: forall a. Ancillary a => ByteArray -> Int -> Maybe a
peekAncillary cmsgs hdroffs = I.peekCmsg cmsgs hdroffs reader
  where reader l t b o s
          | l == cmsgLevel @a && t == cmsgType @a =
            Just (cmsgPeekData b o s)
          | otherwise = Nothing
-- | Store a control message element at offset in buffer
pokeAncillary :: forall a m. (Ancillary a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
pokeAncillary cmsgs hdroffs andat = I.pokeCmsg cmsgs hdroffs l t (cmsgDataSize andat) (cmsgPokeData andat)
  where l = cmsgLevel @a
        t = cmsgType  @a
-- ^ Warning: The buffer must be big enough to store the control message element

-- | This data type holds an abitrary ancillary data type.
data AncillaryData = AncillaryData
  { ancillarySpace :: !Int
  , ancillaryData  :: !(forall a. Ancillary a => Maybe a)
  , ancillaryWrite :: !(forall m. PrimMonad m => MutableByteArray (PrimState m) -> Int -> m ())
  }

mkAncillaryData :: Ancillary a => a -> AncillaryData
mkAncillaryData a = AncillaryData { ancillarySpace = cmsgSpace a
                                  , ancillaryData  = fromDynamic (toDyn a)
                                  , ancillaryWrite = \ buffer offset -> pokeAncillary buffer offset a
                                  }
-- | ancillaryWrite but without the impredicative type
writeAncillary :: PrimMonad m => AncillaryData -> MutableByteArray (PrimState m) -> Int -> m ()
writeAncillary a = ancillaryWrite a

-- | ancillaryData but without the impredicative type
recoverAncillary :: Ancillary a => AncillaryData -> Maybe a
recoverAncillary a = ancillaryData a

{-# INLINE encodeAncillaryData #-}
-- | Encode a list of control message elements
encodeAncillaryData :: [AncillaryData] -> ByteArray
encodeAncillaryData cmsgs = runByteArray $ do
  buffer <- newByteArray size
  zipWithM_ (`writeAncillary` buffer) cmsgs offsets
  return buffer
  where (offsets, size) = (\ a -> (init a, last a)) (scanl (+) 0 (map ancillarySpace cmsgs))
-- ^ Used internally

{-# INLINE decodeAncillaryData #-}
-- | Decode a buffer of control message elements. NOTE: The bytearray must be shrunk to `msg_controllen` bytes before being passed to this function.
decodeAncillaryData :: ByteArray -> [AncillaryData]
decodeAncillaryData cmsgs = fix (\ as offs ->
  if offs < size then
    let space = I.peekCmsg cmsgs offs (\ _ _ _ _ -> I.cmsgSpace)
        andat = AncillaryData { ancillarySpace = space 
                              , ancillaryData  = peekAncillary cmsgs offs
                              , ancillaryWrite = \ dest destOffs -> copyByteArray dest destOffs cmsgs offs space
                              }
    in  andat:as (offs + space)
  else []) 0
  where size = sizeofByteArray cmsgs
-- ^ Used internally. Every AncillaryData element holds a reference to the bytearray and keeps it alive.

-- | Transfer file descriptors between sockets
newtype ScmRights = ScmRights [Fd]
instance Ancillary ScmRights where
  type Anci ScmRights = UNIX_CONTROL_MESSAGE_RIGHTS
  cmsgDataSize (ScmRights fds) = sizeOfType @Fd * length fds
  cmsgPokeData (ScmRights fds) buffer initOffs = void (foldlM (\ offs fd -> writeUnalignedByteArray buffer offs fd >> return (offs + sizeOfType @Fd)) initOffs fds)
  cmsgPeekData buffer o s = assert (s `mod` sizeOfType @Fd == 0) $ ScmRights $ fix (\ as offs size ->
    if size > 0 then
      indexUnalignedByteArray buffer offs:as (offs + sizeOfType @Fd) (size - sizeOfType @Fd)
    else
      []) o s
