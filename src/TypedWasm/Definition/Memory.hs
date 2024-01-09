module TypedWasm.Definition.Memory where

import GHC.Generics (Generic)
import TypedWasm.Util.Enum (Enumerable)

data Signed = Singed | Unsigned
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)
