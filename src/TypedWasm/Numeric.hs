-- Singletons auto generation creates unused top binds, so we ignore such
-- warnings in this module
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypedWasm.Numeric (
    -- | Numeric types and values
    NumericType (..),
    SNumericType (..),
    NumericVal (..),
    -- | Mutability
    Mutability (..),
    SMutability (..),
)
where

import Data.Int (Int32)
import Data.Singletons (SingI (..))
import Data.Singletons.TH (singletons)
import Data.String.Singletons (
    PIsString (FromString),
    SIsString (sFromString),
 )
import Prelude.Singletons (
    FalseSym0,
    PEq (type (==)),
    PShow (ShowsPrec),
    SBool (SFalse, STrue),
    SEq ((%==)),
    SShow (sShowsPrec),
    ShowStringSym0,
    TrueSym0,
    sShowString,
 )
import Prelude hiding (id, (.))

$( singletons
    [d|
        data NumericType = I32 | I64 | F32 | F64
            deriving (Eq, Show)
        |]
 )

$( singletons
    [d|
        data Mutability = Immutable | Mutable
            deriving (Eq, Show)
        |]
 )

-- | The Haskell representation of WASM stack values
data NumericVal (n :: NumericType) where
    NVI32 :: Int32 -> NumericVal 'I32
    NVI64 :: Integer -> NumericVal 'I64
    NVF32 :: Float -> NumericVal 'F32
    NVF64 :: Double -> NumericVal 'F64

deriving instance Eq (NumericVal n)
deriving instance Show (NumericVal n)

-- | We need the `SingI` instance here so that we can construct `NumericValue`s with `fromInteger`
instance (SingI n) => Num (NumericVal n) where
    fromInteger x = case sing @n of
        SI32 -> NVI32 $ fromInteger x
        SI64 -> NVI64 $ fromInteger x
        SF32 -> NVF32 $ fromInteger x
        SF64 -> NVF64 $ fromInteger x
    NVI32 a + NVI32 b = NVI32 (a + b)
    NVI64 a + NVI64 b = NVI64 (a + b)
    NVF32 a + NVF32 b = NVF32 (a + b)
    NVF64 a + NVF64 b = NVF64 (a + b)
    NVI32 a * NVI32 b = NVI32 (a * b)
    NVI64 a * NVI64 b = NVI64 (a * b)
    NVF32 a * NVF32 b = NVF32 (a * b)
    NVF64 a * NVF64 b = NVF64 (a * b)
    NVI32 a - NVI32 b = NVI32 (a - b)
    NVI64 a - NVI64 b = NVI64 (a - b)
    NVF32 a - NVF32 b = NVF32 (a - b)
    NVF64 a - NVF64 b = NVF64 (a - b)
    abs (NVI32 a) = NVI32 (abs a)
    abs (NVI64 a) = NVI64 (abs a)
    abs (NVF32 a) = NVF32 (abs a)
    abs (NVF64 a) = NVF64 (abs a)
    signum (NVI32 a) = NVI32 (signum a)
    signum (NVI64 a) = NVI64 (signum a)
    signum (NVF32 a) = NVF32 (signum a)
    signum (NVF64 a) = NVF64 (signum a)
