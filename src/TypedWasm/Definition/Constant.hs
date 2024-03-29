module TypedWasm.Definition.Constant where

import Data.Int (Int32, Int64)
import TypedWasm.Definition.Types

{- | A haskell representation of a `ValueType`

For terms with polymorphic @vt@, this is an interaction of different numeric types,
so we can defined a number of typeclass instances for manipulating `ConstantRep`s in
haskell
-}
data ConstantRep (vt :: ValueType) where
    CRI32 :: Int32 -> ConstantRep 'I32
    CRI64 :: Int64 -> ConstantRep 'I64
    CRF32 :: Float -> ConstantRep 'F32
    CRF64 :: Double -> ConstantRep 'F64

deriving instance Eq (ConstantRep vt)
deriving instance Ord (ConstantRep vt)
deriving instance Show (ConstantRep vt)

typeOfConstant :: ConstantRep vt -> SNumericType vt
typeOfConstant (CRI32 _) = SNI32
typeOfConstant (CRF32 _) = SNF32
typeOfConstant (CRI64 _) = SNI64
typeOfConstant (CRF64 _) = SNF64

instance (SingNumericType vt) => Num (ConstantRep vt) where
    fromInteger a = case singNumericType @vt of
        SNI32 -> CRI32 $ fromInteger a
        SNI64 -> CRI64 $ fromInteger a
        SNF32 -> CRF32 $ fromInteger a
        SNF64 -> CRF64 $ fromInteger a
    CRI32 a + CRI32 b = CRI32 (a + b)
    CRI64 a + CRI64 b = CRI64 (a + b)
    CRF32 a + CRF32 b = CRF32 (a + b)
    CRF64 a + CRF64 b = CRF64 (a + b)

    CRI32 a * CRI32 b = CRI32 (a * b)
    CRI64 a * CRI64 b = CRI64 (a * b)
    CRF32 a * CRF32 b = CRF32 (a * b)
    CRF64 a * CRF64 b = CRF64 (a * b)

    CRI32 a - CRI32 b = CRI32 (a - b)
    CRI64 a - CRI64 b = CRI64 (a - b)
    CRF32 a - CRF32 b = CRF32 (a - b)
    CRF64 a - CRF64 b = CRF64 (a - b)

    signum (CRI32 a) = CRI32 (signum a)
    signum (CRI64 a) = CRI64 (signum a)
    signum (CRF32 a) = CRF32 (signum a)
    signum (CRF64 a) = CRF64 (signum a)

    abs (CRI32 a) = CRI32 (abs a)
    abs (CRI64 a) = CRI64 (abs a)
    abs (CRF32 a) = CRF32 (abs a)
    abs (CRF64 a) = CRF64 (abs a)

instance (SingNumericType vt) => Real (ConstantRep vt) where
    toRational (CRI32 a) = toRational a
    toRational (CRF32 a) = toRational a
    toRational (CRI64 a) = toRational a
    toRational (CRF64 a) = toRational a

instance (SingFloatingType vt) => Fractional (ConstantRep vt) where
    a / b = case (singFloating @vt, a, b) of
        (SF32, CRF32 x, CRF32 y) -> CRF32 (x / y)
        (SF64, CRF64 x, CRF64 y) -> CRF64 (x / y)
    fromRational r = case singFloating @vt of
        SF32 -> CRF32 $ fromRational r
        SF64 -> CRF64 $ fromRational r
