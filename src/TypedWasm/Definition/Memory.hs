module TypedWasm.Definition.Memory where

import GHC.Generics (Generic)
import TypedWasm.Definition.Types
import TypedWasm.Util.Enum

data Signed = Signed | Unsigned
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

data MemLoadInstruction (vt :: ValueType) where
    MLISelf :: SNumericType vt -> MemLoadInstruction vt
    MLIInt8 :: SIntegralType vt -> Signed -> MemLoadInstruction vt
    MLIInt16 :: SIntegralType vt -> Signed -> MemLoadInstruction vt
    MLIInt32 :: Signed -> MemLoadInstruction 'I64

deriving instance Show (MemLoadInstruction vt)
deriving instance Eq (MemLoadInstruction vt)

instance (SingNumericType vt) => Enumerable (MemLoadInstruction vt) where
    allMembers = case singNumericType @vt of
        SNI32 -> MLISelf SNI32 : foldMap (\s -> [MLIInt8 SI32 s, MLIInt16 SI32 s]) allMembers
        SNI64 ->
            MLISelf SNI64
                : foldMap
                    (\s -> [MLIInt8 SI64 s, MLIInt16 SI64 s, MLIInt32 s])
                    allMembers
        SNF32 -> [MLISelf SNF32]
        SNF64 -> [MLISelf SNF64]

data MemStoreInstruction (vt :: ValueType) where
    MSISelf :: SNumericType vt -> MemStoreInstruction vt
    MSIInt8 :: SIntegralType vt -> MemStoreInstruction vt
    MSIInt16 :: SIntegralType vt -> MemStoreInstruction vt
    MSIInt32 :: MemStoreInstruction 'I64

deriving instance Show (MemStoreInstruction vt)
deriving instance Eq (MemStoreInstruction vt)

instance (SingNumericType vt) => Enumerable (MemStoreInstruction vt) where
    allMembers = case singNumericType @vt of
        SNI32 -> [MSISelf SNI32, MSIInt8 SI32, MSIInt16 SI32]
        SNI64 -> [MSISelf SNI64, MSIInt8 SI64, MSIInt16 SI64, MSIInt32]
        SNF32 -> [MSISelf SNF32]
        SNF64 -> [MSISelf SNF64]

data MemInstruction = MISize | MIGrow
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)
