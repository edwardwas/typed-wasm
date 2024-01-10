module TypedWasm.Definition.Memory where

import Data.Singletons.TH
import Data.Word (Word32)
import GHC.Generics (Generic)
import Prelude.Singletons
import TypedWasm.Definition.Types
import TypedWasm.Util.Enum (Enumerable)

data Signed = Signed | Unsigned
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

$( singletons
    [d|
        data Alignment = A8 | A16 | A32 | A64
            deriving (Eq, Ord)
        |]
 )

deriving instance Show Alignment

type family MaxAlignment (vt :: ValueType) where
    MaxAlignment I32 = A32
    MaxAlignment F32 = A32
    MaxAlignment I64 = A64
    MaxAlignment F64 = A64

type family IfTy a b c where
    IfTy 'True b _ = b
    IfTy 'False _ c = c

type SignNeeded (a :: Alignment) (vt :: ValueType) = IfTy (a == MaxAlignment vt) () Signed

data BitWidth (vt :: ValueType) where
    BitWidthIntegral :: SIntegralType vt -> Sing (a :: Alignment) -> SignNeeded a vt -> BitWidth vt
    BitWidthFloating :: SFloatingType vt -> Sing (a :: Alignment) -> BitWidth vt

selfBitWidth :: forall vt. (SingNumericType vt) => BitWidth vt
selfBitWidth = case singNumericType @vt of
    SNI32 -> BitWidthIntegral SI32 SA32 ()
    SNI64 -> BitWidthIntegral SI64 SA64 ()
    SNF32 -> BitWidthFloating SF32 SA32
    SNF64 -> BitWidthFloating SF64 SA64

data MemoryArgument vt where
    MemoryArgument :: ((a <= MaxAlignment vt) ~ True) => Word32 -> Sing (a :: Alignment) -> MemoryArgument vt

selfMemoryArgument :: forall vt. (SingNumericType vt) => Word32 -> MemoryArgument vt
selfMemoryArgument offset = case singNumericType @vt of
    SNI32 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNI64 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNF32 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNF64 -> MemoryArgument offset (sing @(MaxAlignment vt))
