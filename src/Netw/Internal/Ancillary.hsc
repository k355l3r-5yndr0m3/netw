{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Netw.Internal.Ancillary where
import GHC.TypeLits
import Data.Proxy
#include <sys/socket.h>

type UNIX_CONTROL_MESSAGE_RIGHTS = '(#{const SOL_SOCKET}, #{const SCM_RIGHTS})

class KnownNatPair (p :: (Nat, Nat)) where
  {-# MINIMAL fstNatVal, sndNatVal | natPairVal #-}
  natPairVal :: (Integer, Integer)
  natPairVal = (fstNatVal @p, sndNatVal @p)

  fstNatVal :: Integer
  fstNatVal = fst (natPairVal @p)

  sndNatVal :: Integer
  sndNatVal = snd (natPairVal @p)

instance (KnownNat a, KnownNat b) => KnownNatPair '(a, b) where
  fstNatVal = natVal @a Proxy
  sndNatVal = natVal @b Proxy
