{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Socket (
  FlowInfo,
  ScopeID,
  HostAddress,
  HostAddress6,
  SockAddr (..),
  PortNumber (),
  hostAddress6ToTuple,
  tupleToHostAddress6,
  tupleToHostAddress,
  isSupportedSockAddr,
) where

import Control.DeepSeq (NFData)
import Data.Word
import Foreign
import GHC.Generics (Generic)

-- | Flow information.
type FlowInfo = Word32

-- | Scope identifier.
type ScopeID = Word32

{- | The raw network byte order number is read using host byte order.
Therefore on little-endian architectures the byte order is swapped. For
example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
and as @0x7f000001@ on big-endian hosts.

For direct manipulation prefer 'hostAddressToTuple' and
'tupleToHostAddress'.
-}
type HostAddress = Word32

{- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.

For direct manipulation prefer 'hostAddress6ToTuple' and
'tupleToHostAddress6'.
-}
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts 'HostAddress6' to representation-independent IPv6 octuple.

{- -- prop> (w1,w2,w3,w4,w5,w6,w7,w8) == hostAddress6ToTuple (tupleToHostAddress6 (w1,w2,w3,w4,w5,w6,w7,w8)) -}
hostAddress6ToTuple ::
  HostAddress6 ->
  ( Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  )
hostAddress6ToTuple (w3, w2, w1, w0) =
  let high, low :: Word32 -> Word16
      high w = fromIntegral (w `shiftR` 16)
      low w = fromIntegral w
   in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | Converts IPv6 octuple to 'HostAddress6'.
tupleToHostAddress6 ::
  ( Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  , Word16
  ) ->
  HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
  let add :: Word16 -> Word16 -> Word32
      high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
   in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

data SockAddr
  = SockAddrInet PortNumber HostAddress
  | SockAddrInet6 PortNumber FlowInfo HostAddress6 ScopeID
  | -- | The path must have fewer than 104 characters. All of these characters must have code points less than 256.
    SockAddrUnix String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Is the socket address type supported on this system?
isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr addr = case addr of
  SockAddrInet {} -> True
  SockAddrInet6 {} -> True
  SockAddrUnix {} -> True

{- | Port number.
  Use the @Num@ instance (i.e. use a literal) to create a
  @PortNumber@ value.

>>> 1 :: PortNumber
1
>>> read "1" :: PortNumber
1
>>> show (12345 :: PortNumber)
"12345"
>>> 50000 < (51000 :: PortNumber)
True
>>> 50000 < (52000 :: PortNumber)
True
>>> 50000 + (10000 :: PortNumber)
60000
-}
newtype PortNumber = PortNum Word16
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Enum, Bounded, Real, Integral)
  deriving newtype (NFData)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
  let x `sl` i = fromIntegral x `shiftL` i :: Word32
   in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32
