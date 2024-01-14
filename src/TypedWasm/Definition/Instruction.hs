module TypedWasm.Definition.Instruction where

import Control.Category
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import GHC.Generics (Generic)
import TypedWasm.Definition.Memory
import TypedWasm.Definition.Types
import TypedWasm.Util.Enum
import TypedWasm.Util.List
import Prelude hiding (id, (.))

-- | A haskell representation of a `ValueType`
data ConstantRep (vt :: ValueType) where
    CRI32 :: Int32 -> ConstantRep 'I32
    CRI64 :: Int64 -> ConstantRep 'I64
    CRF32 :: Float -> ConstantRep 'F32
    CRF64 :: Double -> ConstantRep 'F64

deriving instance Eq (ConstantRep vt)
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

-- | Unary operations on integeral values
data IntegralUnaryOp
    = IUOCountLeadingZeros
    | IUOCountTrailingZeros
    | IUOPopulationCount
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

-- | Binary operations on integral values
data IntegralBinaryOp
    = IBOAdd
    | IBOSub
    | IBOMul
    | IBODiv Signed
    | IBORem Signed
    | IBOAnd
    | IBOOr
    | IBOXor
    | IBOShiftLeft
    | IBOShiftRight Signed
    | IBORotateLeft
    | IBORotateRight
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

-- | Unary operations on floating values
data FloatingUnaryOp
    = FUOAbs
    | FUONeg
    | FUOSqrt
    | FUOCeil
    | FUOFloor
    | FUOTruncate
    | FUONearest
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

-- | Binary operations on floating values
data FloatingBinaryOp
    = FBOAdd
    | FBOSub
    | FBOMul
    | FBODiv
    | FBOMin
    | FBOMax
    | FBOCopySign
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

-- | Comparisons on integral values
data IntegralComparison
    = ICEqual
    | ICNotEqual
    | ICLessThan Signed
    | ICGreaterThan Signed
    | ICLessThanOrEq Signed
    | ICGreaterThanOrEq Signed
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

{- | Tag whether a reference is mutable or not.

This is often used at the type level
-}
data Mutability = Immutable | Mutable
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Enumerable)

class WasmTarget wt where
    data TargetJumpLabel wt :: [ValueType] -> Type
    data TargetFunc wt :: [ValueType] -> [ValueType] -> Type
    data TargetRef wt :: Mutability -> ValueType -> Type

{- | An instruction (or series of instructions) to be executed.

The three type variables are as follows:

* @wt@ is the "wasm target". This should be a member of the class `WasmTarget`.
This captures all the required types that a wasm implementation may need
access to.
* `is` is the the of the stack before this instruction is executed.
* `os` is the the of the stack after this instruction is executed.

When writing `Instruction`s, one should keep @wt@ polymorphic. This allows
callers to use different targets. Note that this means that the only way to
introduce `TargetRef`s and the like is by callbacks in `Instruction`s, which
keeps this type safe
-}
data Instruction (wt :: Type) (is :: [ValueType]) (os :: [ValueType]) where
    -- | Perform one action after the other.
    InstrSequence ::
        (DifferenceList bs cs) =>
        Instruction wt as bs ->
        Instruction wt cs ds ->
        Instruction wt as (ConcatList (RemainingList bs cs) ds)
    -- | Perform no actions
    InstrNOP :: Instruction wt '[] '[]
    -- | Indicate that this code should never be ran
    InstrUnreachable :: Instruction wt as bs
    -- | Push a constant onto the stack
    InstrConst :: SNumericType t -> ConstantRep t -> Instruction wt '[] '[t]
    -- | Perform an integral unary operation
    InstrIntegralUnary ::
        SIntegralType t ->
        IntegralUnaryOp ->
        Instruction wt '[t] '[t]
    -- | Perform an integral binary operation
    InstrIntegralBinary ::
        SIntegralType t ->
        IntegralBinaryOp ->
        Instruction wt '[t, t] '[t]
    -- | Compare two integral values
    InstrIntegralCompare ::
        SIntegralType t ->
        IntegralComparison ->
        Instruction wt '[t, t] '[I32]
    -- | Check if a value equals zero
    InstrEqualZero ::
        SIntegralType t ->
        Instruction wt '[t] '[I32]
    -- | Perform an floating unary operation
    InstrFloatingUnary ::
        SFloatingType t ->
        FloatingUnaryOp ->
        Instruction wt '[t] '[t]
    -- | Perform an floating binary operation
    InstrFloatingBinary ::
        SFloatingType t ->
        FloatingBinaryOp ->
        Instruction wt '[t, t] '[t]
    -- | Break and jump to this jump label. Instructions after this point will not be ran
    InstrBreak :: TargetJumpLabel wt xs -> Instruction wt xs '[]
    -- | Construct a loop.
    --
    -- This will run the `Instruction`s created by the callback. Inside this callback jumping to
    -- the provided label will restart the loop
    InstrLoop ::
        forall is os wt.
        (SingList SingNumericType is, SingList SingNumericType os) =>
        (TargetJumpLabel wt is -> Instruction wt is os) ->
        Instruction wt is os
    -- | If the top of the stack is non-zero, execute the first `Instruction`.
    -- Otherwise execute the second
    InstrIf ::
        (SingList SingNumericType is, SingList SingNumericType os) =>
        Instruction wt is os ->
        Instruction wt is os ->
        Instruction wt ('I32 ': is) os
    -- | Put the value of a ref on stack
    InstrGetRef :: TargetRef wt mut t -> Instruction wt '[] '[t]
    -- | Update the value of a  ref
    InstrSetRef :: TargetRef wt 'Mutable t -> Instruction wt '[t] '[]
    -- | Load something from memory
    InstrLoadMemory :: MemLoadInstruction t -> Instruction wt '[I32] '[t]
    -- | Write something to memory
    InstrStoreMemory :: MemStoreInstruction t -> Instruction wt '[I32, t] '[]

-- | An infix definition of `InstrSequence`
(>.) ::
    (DifferenceList bs cs) =>
    Instruction wt as bs ->
    Instruction wt cs ds ->
    Instruction wt as (ConcatList (RemainingList bs cs) ds)
(>.) = InstrSequence

infixl 3 >.

instance
    (DifferenceList as as, RemainingList as as ~ '[]) =>
    Semigroup (Instruction wt as as)
    where
    InstrNOP <> a = a
    a <> InstrNOP = a
    a <> b = a >. b

instance (as ~ '[], bs ~ '[]) => Monoid (Instruction wt as bs) where
    mempty = InstrNOP

instance (SingNumericType t, is ~ '[], os ~ '[t]) => Num (Instruction wt is os) where
    fromInteger = case singNumericType @t of
        SNI32 -> instrConst . CRI32 . fromInteger
        SNI64 -> instrConst . CRI64 . fromInteger
        SNF32 -> instrConst . CRF32 . fromInteger
        SNF64 -> instrConst . CRF64 . fromInteger
    a + b = a >. b >. instrAdd
    a * b = a >. b >. instrMul
    a - b = a >. b >. instrSub
    abs x = case singNumericType @t of
        SNI32 -> undefined
        SNI64 -> undefined
        SNF32 -> x >. InstrFloatingUnary SF32 FUOAbs
        SNF64 -> x >. InstrFloatingUnary SF64 FUOAbs
    signum x = case singNumericType @t of
        SNI32 -> undefined
        SNI64 -> undefined
        SNF32 -> x >. 1 >. InstrFloatingBinary SF32 FBOCopySign
        SNF64 -> x >. 1 >. InstrFloatingBinary SF64 FBOCopySign

{- | Create a constant instruction.

This is polymorphic in haskell, but will be monomorphised in WASM
-}
instrConst :: (SingNumericType t) => ConstantRep t -> Instruction wt '[] '[t]
instrConst = InstrConst singNumericType

{- | A helper function for creating instructions that are polymorphic over
numeric WASM types
-}
binaryInstrIntOrFloat ::
    forall t wt.
    (SingNumericType t) =>
    (forall x. SIntegralType x -> Instruction wt '[x, x] '[x]) ->
    (forall x. SFloatingType x -> Instruction wt '[x, x] '[x]) ->
    Instruction wt '[t, t] '[t]
binaryInstrIntOrFloat intInstr floatInstr = case singNumericType @t of
    SNI32 -> intInstr SI32
    SNI64 -> intInstr SI64
    SNF32 -> floatInstr SF32
    SNF64 -> floatInstr SF64

-- | A polymorphic add function
instrAdd :: forall t wt. (SingNumericType t) => Instruction wt '[t, t] '[t]
instrAdd =
    binaryInstrIntOrFloat
        (`InstrIntegralBinary` IBOAdd)
        (`InstrFloatingBinary` FBOAdd)

-- | A polymorphic multiply function
instrMul :: forall t wt. (SingNumericType t) => Instruction wt '[t, t] '[t]
instrMul =
    binaryInstrIntOrFloat
        (`InstrIntegralBinary` IBOMul)
        (`InstrFloatingBinary` FBOMul)

-- | A polymorphic subtraction function
instrSub :: forall t wt. (SingNumericType t) => Instruction wt '[t, t] '[t]
instrSub =
    binaryInstrIntOrFloat
        (`InstrIntegralBinary` IBOSub)
        (`InstrFloatingBinary` FBOSub)

-- | Evaluate the first `Instruction`. If it's non-zero, evaluate the second
instrWhen ::
    Instruction wt '[] '[ 'I32] ->
    Instruction wt '[] '[] ->
    Instruction wt '[] '[]
instrWhen check body = check >. InstrIf body InstrNOP

-- | Polymorphically load a value from memory
instrLoadSelf :: (SingNumericType t) => Instruction wt '[I32] '[t]
instrLoadSelf = InstrLoadMemory (MLISelf singNumericType)

-- | Polymorphically write a value to memory
instrStoreSelf :: (SingNumericType t) => Instruction wt '[I32, t] '[]
instrStoreSelf = InstrStoreMemory (MSISelf singNumericType)
