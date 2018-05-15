{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- Some code in this file was adapted from the @network-conduit@ library by
-- Michael Snoyman. Copyright (c) 2011. See its licensing terms (BSD3) at:
--   https://github.com/snoyberg/conduit/blob/master/network-conduit/LICENSE

module Network.Simple.Internal
  ( HostPreference(..)
  , hpHostName
  , ipv4mapped_to_ipv4
  , isIPv4addr
  , isIPv6addr
  , prioritize
  , happyEyeballSort
  ) where

import           Data.Bits                     (shiftR, (.&.))
import qualified Data.List                     as List
import           Data.String                   (IsString (fromString))
import           Data.Word                     (byteSwap32)
import qualified Network.Socket as             NS

-- | Preferred host to bind.
data HostPreference
  = HostAny          -- ^Any available host.
  | HostIPv4         -- ^Any available IPv4 host.
  | HostIPv6         -- ^Any available IPv6 host.
  | Host NS.HostName -- ^An explicit host name.
  deriving (Eq, Ord, Show, Read)

-- | The following special values are recognized:
--
-- * @*@ means 'HostAny'
--
-- * @*4@ means 'HostIPv4'
--
-- * @*6@ means 'HostIPv6'
--
-- * Any other string is 'Host'
instance IsString HostPreference where
  fromString "*"  = HostAny
  fromString "*4" = HostIPv4
  fromString "*6" = HostIPv6
  fromString s    = Host s

-- | Extract the 'NS.HostName' from a 'Host' preference, or 'Nothing' otherwise.
hpHostName:: HostPreference -> Maybe NS.HostName
hpHostName (Host s) = Just s
hpHostName _        = Nothing

-- | Convert IPv4-Mapped IPv6 Addresses to IPv4.
ipv4mapped_to_ipv4:: NS.SockAddr -> NS.SockAddr
ipv4mapped_to_ipv4 (NS.SockAddrInet6 p _ (0, 0, 0xFFFF, h) _)
  = NS.SockAddrInet p (NS.tupleToHostAddress
      (fromIntegral (shiftR (h .&. 0xFF000000) 24),
       fromIntegral (shiftR (h .&. 0x00FF0000) 16),
       fromIntegral (shiftR (h .&. 0x0000FF00) 8),
       fromIntegral         (h .&. 0x000000FF)))
ipv4mapped_to_ipv4 sa = sa

-- | Given a list of 'NS.AddrInfo's, reorder it so that ipv6 and ipv4 addresses,
-- when available, are intercalated, with a ipv6 address first.
happyEyeballSort :: [NS.AddrInfo] -> [NS.AddrInfo]
happyEyeballSort l =
    concat (List.transpose ((\(a,b) -> [a,b]) (List.partition isIPv6addr l)))

isIPv4addr :: NS.AddrInfo -> Bool
isIPv4addr x = NS.addrFamily x == NS.AF_INET

isIPv6addr :: NS.AddrInfo -> Bool
isIPv6addr x = NS.addrFamily x == NS.AF_INET6

-- | Move the elements that match the predicate closer to the head of the list.
-- Sorting is stable.
prioritize :: (a -> Bool) -> [a] -> [a]
prioritize p = uncurry (++) . List.partition p
