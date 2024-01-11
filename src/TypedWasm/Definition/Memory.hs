module TypedWasm.Definition.Memory where

import Data.Bool.Singletons
import Data.Singletons.TH
import Data.String.Singletons
import Data.Text (Text)
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
            deriving (Eq, Ord, Show)
        |]
 )

type family MaxAlignment (vt :: ValueType) where
    MaxAlignment I32 = A32
    MaxAlignment F32 = A32
    MaxAlignment I64 = A64
    MaxAlignment F64 = A64

alignmentSizeString :: Alignment -> Text
alignmentSizeString A8 = "8"
alignmentSizeString A16 = "16"
alignmentSizeString A32 = "32"
alignmentSizeString A64 = "64"

data BitWidth (vt :: ValueType) where
    BitWidthIntegral ::
        (a ~ MaxAlignment vt) =>
        SIntegralType vt ->
        Sing (a :: Alignment) ->
        BitWidth vt
    BitWidthSignedIntegral ::
        ((a < MaxAlignment vt) ~ 'True) =>
        SIntegralType vt ->
        Sing (a :: Alignment) ->
        Signed ->
        BitWidth vt
    BitWidthFloating :: SFloatingType vt -> Sing (a :: Alignment) -> BitWidth vt

selfBitWidth :: forall vt. (SingNumericType vt) => BitWidth vt
selfBitWidth = case singNumericType @vt of
    SNI32 -> BitWidthIntegral SI32 SA32
    SNI64 -> BitWidthIntegral SI64 SA64
    SNF32 -> BitWidthFloating SF32 SA32
    SNF64 -> BitWidthFloating SF64 SA64

data MemoryArgument vt where
    MemoryArgument :: ((a <= MaxAlignment vt) ~ True) => Word32 -> Sing (a :: Alignment) -> MemoryArgument vt

deriving instance Show (MemoryArgument vt)

instance Eq (MemoryArgument vt) where
    MemoryArgument oa aa == MemoryArgument ob ab = case aa %~ ab of
        Proved Refl -> oa == ob
        Disproved _ -> False

memoryArgumentOffSet :: MemoryArgument vt -> Word32
memoryArgumentOffSet (MemoryArgument offset _) = offset

memoryArgumentAlignment :: MemoryArgument vt -> Alignment
memoryArgumentAlignment (MemoryArgument _ a) = fromSing a

selfMemoryArgument :: forall vt. (SingNumericType vt) => Word32 -> MemoryArgument vt
selfMemoryArgument offset = case singNumericType @vt of
    SNI32 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNI64 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNF32 -> MemoryArgument offset (sing @(MaxAlignment vt))
    SNF64 -> MemoryArgument offset (sing @(MaxAlignment vt))

alignmentNumber :: Alignment -> Word32
alignmentNumber A8 = 0
alignmentNumber A16 = 1
alignmentNumber A32 = 2
alignmentNumber A64 = 3

memoryArgumentTuple :: MemoryArgument vt -> (Word32, Word32)
memoryArgumentTuple (MemoryArgument offset sAlign) =
    ( offset
    , alignmentNumber $ fromSing sAlign
    )
