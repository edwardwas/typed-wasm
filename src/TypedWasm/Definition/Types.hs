module TypedWasm.Definition.Types where

{- | All the types of values that can be placed on a WASM stack.

This will often be used at the type level
-}
data ValueType
    = I32
    | I64
    | F32
    | F64
    | ExternRef
    | Func
    deriving stock (Eq, Show)

-- | A run time witness of a `ValueType`
data SValueType (vt :: ValueType) where
    SVI32 :: SValueType 'I32
    SVI64 :: SValueType 'I64
    SVF32 :: SValueType 'F32
    SVF64 :: SValueType 'F64
    SVExternRef :: SValueType 'ExternRef

lowerSValueType :: SValueType vt -> ValueType
lowerSValueType SVI32 = I32
lowerSValueType SVI64 = I64
lowerSValueType SVF32 = F32
lowerSValueType SVF64 = F64
lowerSValueType SVExternRef = ExternRef

-- | Proof that `vt` is a valid `ValueType`
class SingValueType (vt :: ValueType) where
    singValueType :: SValueType vt

instance SingValueType 'I32 where
    singValueType = SVI32
instance SingValueType 'I64 where
    singValueType = SVI64
instance SingValueType 'F32 where
    singValueType = SVF32
instance SingValueType 'F64 where
    singValueType = SVF64
instance SingValueType 'ExternRef where
    singValueType = SVExternRef

-- | A run time witness that the `ValueType` @vt@ is a numeric type
data SNumericType (vt :: ValueType) where
    SNI32 :: SNumericType 'I32
    SNI64 :: SNumericType 'I64
    SNF32 :: SNumericType 'F32
    SNF64 :: SNumericType 'F64

deriving instance Show (SNumericType vt)
deriving instance Eq (SNumericType vt)

class SingNumericType (vt :: ValueType) where
    singNumericType :: SNumericType vt

instance SingNumericType 'I32 where
    singNumericType = SNI32
instance SingNumericType 'I64 where
    singNumericType = SNI64
instance SingNumericType 'F32 where
    singNumericType = SNF32
instance SingNumericType 'F64 where
    singNumericType = SNF64

-- | A run time witness that the `ValueType` @vt@ is an intergral numeric type
data SIntegralType (vt :: ValueType) where
    SI32 :: SIntegralType 'I32
    SI64 :: SIntegralType 'I64

deriving instance Show (SIntegralType vt)
deriving instance Eq (SIntegralType vt)

sIntegralTypeToNumeric :: SIntegralType vt -> SNumericType vt
sIntegralTypeToNumeric SI32 = SNI32
sIntegralTypeToNumeric SI64 = SNI64

class (SingNumericType vt) => SingIntegralType (vt :: ValueType) where
    singIntegral :: SIntegralType vt

instance SingIntegralType 'I32 where
    singIntegral = SI32
instance SingIntegralType 'I64 where
    singIntegral = SI64

-- | A run time witness that the `ValueType` @vt@ is an intergral numeric type
data SFloatingType (vt :: ValueType) where
    SF32 :: SFloatingType 'F32
    SF64 :: SFloatingType 'F64

sFloatingTypeToNumeric :: SFloatingType vt -> SNumericType vt
sFloatingTypeToNumeric SF32 = SNF32
sFloatingTypeToNumeric SF64 = SNF64

class SingNumericType vt => SingFloatingType (vt :: ValueType) where
    singFloating :: SFloatingType vt

instance SingFloatingType 'F32 where
    singFloating = SF32
instance SingFloatingType 'F64 where
    singFloating = SF64
