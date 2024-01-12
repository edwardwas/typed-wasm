module TypedWasm.Definition.Memory where

import GHC.Generics (Generic)
import TypedWasm.Util.Enum

data Signed = Signed | Unsigned
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)
