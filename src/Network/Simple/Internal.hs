{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- Some code in this file was adapted from the @network-conduit@ library by
-- Michael Snoyman. Copyright (c) 2011. See its licensing terms (BSD3) at:
--   https://github.com/snoyberg/conduit/blob/master/network-conduit/LICENSE

module Network.Simple.Internal
  ( HostPreference(..)
  , hpHostName
  , convertIPv6Mapped
  ) where

import           Data.String                   (IsString (fromString))
import qualified Network.Socket as             NS
import           Data.Word                     (byteSwap32)

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

-- | Check an IPv6 and convert it to IPv4 if neccessary
convertIPv6Mapped :: NS.SockAddr -> NS.SockAddr
convertIPv6Mapped (NS.SockAddrInet6 p _ (0, 0, 0xFFFF, h) _) = NS.SockAddrInet p (byteSwap32 h)
convertIPv6Mapped sa = sa

