{-# LANGUAGE DefaultSignatures #-}

module TypedWasm.Util.Enum where

import GHC.Generics

class GEnumerable f where
    gAllMembers :: [f a]

instance (GEnumerable f) => GEnumerable (M1 i c f) where
    gAllMembers = map M1 gAllMembers

instance (GEnumerable f, GEnumerable g) => GEnumerable (f :+: g) where
    gAllMembers = map L1 gAllMembers <> map R1 gAllMembers

instance GEnumerable U1 where
    gAllMembers = [U1]

instance (Enumerable c) => GEnumerable (K1 i c) where
    gAllMembers = map K1 allMembers

class Enumerable a where
    allMembers :: [a]
    default allMembers :: (Generic a, GEnumerable (Rep a)) => [a]
    allMembers = map to gAllMembers
