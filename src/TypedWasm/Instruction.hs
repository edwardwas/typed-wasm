{-# LANGUAGE AllowAmbiguousTypes #-}

module TypedWasm.Instruction where

import Control.Category
import Control.Monad.Free
import Data.Int
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.String.Singletons
import Data.Text (Text)
import Prelude.Singletons
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

type FuncKind = [NumericType] -> [NumericType] -> Type

type RefKind = Mutability -> NumericType -> Type

-- | A type family for combining lists
type family ConcatList as bs where
    ConcatList '[] bs = bs
    ConcatList (a ': as) bs = a ': ConcatList as bs

{- | WASM instructions

More accurately, this is a block of instructions.
-}
data
    Instruction
        (fr :: FuncKind)
        (r :: RefKind)
        (is :: [NumericType])
        (os :: [NumericType])
    where
    InstrConcat :: Instruction fr g as bs -> Instruction fr g bs cs -> Instruction fr g as cs
    InstrNOP :: Instruction fr g xs xs
    InstrDrop :: Instruction fr g (x ': xs) xs
    InstrConst :: SNumericType t -> NumericVal t -> Instruction fr g xs (t ': xs)
    InstrAdd :: SNumericType t -> Instruction fr g (t ': t ': xs) (t ': xs)
    InstrMul :: SNumericType t -> Instruction fr g (t ': t ': xs) (t ': xs)
    InstrSub :: SNumericType t -> Instruction fr g (t ': t ': xs) (t ': xs)
    InstrNegate :: SNumericType t -> Instruction fr g (t ': xs) (t ': xs)
    InstrAbs :: SNumericType t -> Instruction fr g (t ': xs) (t ': xs)
    InstrGlobalGet :: g m t -> Instruction fr g xs (t ': xs)
    InstrGlobalSet :: g 'Mutable t -> Instruction fr g (t ': xs) xs
    InstrCallFunc ::
        (SingI is, SingI os) =>
        Sing xs ->
        fr is os ->
        Instruction fr g (ConcatList is xs) (ConcatList os xs)
    InstrIf :: Instruction fr g is os -> Instruction fr g is os -> Instruction fr g ('I32 ': is) os

instance Category (Instruction fr global) where
    id = InstrNOP
    (.) = flip InstrConcat

instrConst :: (SingI t) => NumericVal t -> Instruction f g xs (t ': xs)
instrConst n = InstrConst sing n

instrAdd :: (SingI t) => Instruction f g (t ': t ': xs) (t ': xs)
instrAdd = InstrAdd sing

class EmptyOrSingle xs
instance EmptyOrSingle '[]
instance EmptyOrSingle '[x]

data FunctionDefinition f r is os where
    FDWithLocal ::
        (SingI t) =>
        (r m t -> FunctionDefinition f r is os) ->
        FunctionDefinition f r is os
    FDWithParam ::
        (r m t -> FunctionDefinition f r is os) ->
        FunctionDefinition f r (t ': is) os
    FDBody :: Instruction f r '[] os -> FunctionDefinition f r '[] os

data ModuleBuilderF (fr :: FuncKind) (global :: RefKind) (a :: Type) where
    AddGlobal :: SMutability m -> NumericVal t -> (g m t -> a) -> ModuleBuilderF fr g a
    DeclareFunc ::
        (EmptyOrSingle os) =>
        FunctionDefinition f r is os ->
        (f is os -> a) ->
        ModuleBuilderF f r a
    ExportFunction :: Text -> fr is os -> a -> ModuleBuilderF fr g a

instance Functor (ModuleBuilderF fr g) where
    fmap f (AddGlobal s n k) = AddGlobal s n (f <$> k)
    fmap f (DeclareFunc is k) = DeclareFunc is (f <$> k)
    fmap f (ExportFunction name fr a) = ExportFunction name fr (f a)

type ModuleBuilder fr g = Free (ModuleBuilderF fr g)

addImmutableGlobal :: NumericVal t -> ModuleBuilder fr g (g 'Immutable t)
addImmutableGlobal n = liftF $ AddGlobal sing n id

addMutableGlobal :: NumericVal t -> ModuleBuilder fr g (g 'Mutable t)
addMutableGlobal n = liftF $ AddGlobal sing n id

declareFunc :: (EmptyOrSingle os) => (MonadFree (ModuleBuilderF f r) m) => FunctionDefinition f r is os -> m (f is os)
declareFunc func = liftF $ DeclareFunc func id

exportFunc :: Text -> fr is os -> ModuleBuilder fr g ()
exportFunc txt fr = liftF $ ExportFunction txt fr ()
