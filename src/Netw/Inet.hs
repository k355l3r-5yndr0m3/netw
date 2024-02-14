{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
module Netw.Inet where
import Netw.Internal.Type
import Netw.Internal.Port

import Data.Bits
import Data.Primitive.ByteArray.Unaligned
import Numeric

import GHC.Exts
import GHC.ByteOrder
import GHC.Word

-- Conversion from host endianness to network endianness (BE)

-- | Convert 16-bit word from host endianness to network endianness (Big endian)
hton16# :: Word16# -> Word16#
-- | Convert 32-bit word from host endianness to network endianness (Big endian)
hton32# :: Word32# -> Word32#
-- | Convert 64-bit word from host endianness to network endianness (Big endian)
hton64# :: Word64# -> Word64#

hton16# = $(case targetByteOrder of LittleEndian -> [| \ w# -> wordToWord16# (byteSwap16# (word16ToWord# w#)) |]; BigEndian -> [| \ w# -> w# |])
hton32# = $(case targetByteOrder of LittleEndian -> [| \ w# -> wordToWord32# (byteSwap32# (word32ToWord# w#)) |]; BigEndian -> [| \ w# -> w# |])
hton64# = $(case targetByteOrder of LittleEndian -> [| byteSwap64# |]; BigEndian -> [| \ w# -> w# |])

-- | Convert 16-bit word from host endianness to network endianness (Big endian)
hton16 :: Word16 -> Word16
hton16 (W16# w) = W16# (hton16# w)

-- | Convert 32-bit word from host endianness to network endianness (Big endian)
hton32 :: Word32 -> Word32
hton32 (W32# w) = W32# (hton32# w)

-- | Convert 64-bit word from host endianness to network endianness (Big endian)
hton64 :: Word64 -> Word64
hton64 (W64# w) = W64# (hton64# w)

-- Conversion from network endianness (BE) to host endianness
-- | Convert 16-bit word from network endianness (Big endian) to host endianness
ntoh16# :: Word16# -> Word16#
-- | Convert 32-bit word from network endianness (Big endian) to host endianness
ntoh32# :: Word32# -> Word32#
-- | Convert 64-bit word from network endianness (Big endian) to host endianness
ntoh64# :: Word64# -> Word64#

ntoh16# = $(case targetByteOrder of LittleEndian -> [| \ w# -> wordToWord16# (byteSwap16# (word16ToWord# w#)) |]; BigEndian -> [| \ w# -> w# |])
ntoh32# = $(case targetByteOrder of LittleEndian -> [| \ w# -> wordToWord32# (byteSwap32# (word32ToWord# w#)) |]; BigEndian -> [| \ w# -> w# |])
ntoh64# = $(case targetByteOrder of LittleEndian -> [| byteSwap64# |]; BigEndian -> [| \ w# -> w# |])

-- | Convert 16-bit word from network endianness (Big endian) to host endianness
ntoh16 :: Word16 -> Word16
ntoh16 (W16# w) = W16# (ntoh16# w)

-- | Convert 32-bit word from network endianness (Big endian) to host endianness
ntoh32 :: Word32 -> Word32
ntoh32 (W32# w) = W32# (ntoh32# w)

-- | Convert 64-bit word from network endianness (Big endian) to host endianness
ntoh64 :: Word64 -> Word64
ntoh64 (W64# w) = W64# (ntoh64# w)

-- | Get the port number in network byteorder 
getPortInBE :: Port -> Word16
getPortInBE (Port p) = hton16 p

-- | Get the port number from network byteorder
portFromBE :: Word16 -> Port
portFromBE = Port . ntoh16


-- | Evaluate to True if port is reserved for super user use.
isReserved :: Port -> Bool
isReserved (Port p) = p < _IPPORT_RESERVED

-- | Evaluate to True if port is reserved for explicit use and is never automatically allocated.
isUserReserved :: Port -> Bool
isUserReserved (Port p) = p >= _IPPORT_USERRESERVED
-- NOTE: Check if >= or > is correct since documents conflict

-- | Port number zero. The OS will assign a random port.
pattern PortRandom :: Port
pattern PortRandom = Port 0

-- | IP version 4 address. Stored in network byteorder.
newtype InAddr = InAddr { inAddrContent :: Word32 } deriving PrimUnaligned

-- | 0.0.0.0
pattern InAddrAny :: InAddr
pattern InAddrAny = InAddr 0

-- | 255.255.255.255
pattern InAddrBroadcast :: InAddr
pattern InAddrBroadcast = InAddr 0xff_ff_ff_ff

-- | 127.0.0.1
pattern InAddrLoopback :: InAddr
pattern InAddrLoopback = InAddr $(case targetByteOrder of LittleEndian -> [p| 0x01_00_00_7f |]; BigEndian -> [p| 0x7f_00_00_01 |])

-- | IP version 6 address. Stored in network byteorder
data In6Addr = In6Addr# (# Word64#, Word64# #)
-- ^ In6Addr# (# high, low #)

-- | Make an IP version 4 address
ip :: Word8 -> Word8 -> Word8 -> Word8 -> InAddr
ip (fromIntegral -> _0) (fromIntegral -> _1) (fromIntegral -> _2) (fromIntegral -> _3) =
  InAddr (shiftL _0 24 .|. shiftL _1 16 .|. shiftL _2 8 .|. shiftL _3 0)

-- | View an IP version 4 address
unIp :: InAddr -> (Word8, Word8, Word8, Word8)
unIp (InAddr addr) = (fromIntegral _0, fromIntegral _1, fromIntegral _2, fromIntegral _3)
  where _0 = shiftR addr 24
        _1 = shiftR addr 16
        _2 = shiftR addr 8
        _3 = shiftR addr 0
instance Show InAddr where
  show (unIp -> (_0, _1, _2, _3)) = show _0 ++ '.' : show _1 ++ '.' : show _2 ++ '.' : show _3

-- | Make an IP version 6 address
ip6 :: Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> Word16 -> In6Addr
ip6 = ip6'
  where cv :: Int -> Word16 -> Word64
        cv n = (`shiftL` (n * 16)) . fromIntegral . hton16
        ip6' (cv 3 -> _0) (cv 2 -> _1) (cv 1 -> _2) (cv 0 -> _3) (cv 3 -> _4) (cv 2 -> _5) (cv 1 -> _6) (cv 0 -> _7) =
          let !(W64# high#) = _0 .|. _1 .|. _2 .|. _3
              !(W64# low#)  = _4 .|. _5 .|. _6 .|. _7
          in  In6Addr# (# high#, low# #)

-- | View an IP version 6 address
unIp6 :: In6Addr -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
unIp6 (In6Addr# (# high#, low# #)) = (vc 3 high#, vc 2 high#, vc 1 high#, vc 0 high#, vc 3 low#, vc 2 low#, vc 1 low#, vc 0 low#)
  where vc :: Int -> Word64# -> Word16
        vc n (W64# -> w) = fromIntegral (shiftR w (8 * n))
instance Show In6Addr where
  showsPrec _ addr = foldr (\ a as -> showHex a . (':' :) . as ) id groups
    where groups = let (a, b, c, d, e, f, g, h) = unIp6 addr in [a, b, c, d, e, f, g, h]


-- |
-- Port number in internet addresses.
-- The port number is stored in host byteorder and is converted into network
-- byteorder when needed.
newtype Port = Port Word16 deriving (Eq, Ord, Num)

-- Ports
$(declarePorts 'Port)
