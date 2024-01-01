module TypedWasm.Instruction where

import Control.Category
import Control.Monad.Cont
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Singletons
import Data.Text (Text)
import TypedWasm.Numeric
import Prelude hiding (id, (.))

type FuncKind = [NumericType] -> Maybe NumericType -> Type

type RefKind = Mutability -> NumericType -> Type

type JumpTargetKind = [NumericType] -> [NumericType] -> Type

class WasmTarget (wt :: Type) where
    type TargetFunction wt :: FuncKind
    type TargetReference wt :: RefKind
    type TargetJumpTarget wt :: JumpTargetKind

-- | A type family for combining lists
type family ConcatList as bs where
    ConcatList '[] bs = bs
    ConcatList (a ': as) bs = a ': ConcatList as bs

-- | If @ma@ is `Just`, prepend it to @bs@. Otherwise this is just @bs@
type family PrependMaybe ma bs where
    PrependMaybe 'Nothing bs = bs
    PrependMaybe ('Just a) bs = a ': bs

data Block (wt :: Type) is o where
    BlockNoReturn :: Instruction wt is '[] -> Block wt is 'Nothing
    BlockSingleReturn :: Sing o -> Instruction wt is '[o] -> Block wt is ('Just o)

blockSingleReturn :: (SingI o) => Instruction wt is '[o] -> Block wt is ('Just o)
blockSingleReturn = BlockSingleReturn sing

data
    Instruction
        (wt :: Type)
        (is :: [NumericType])
        (os :: [NumericType])
    where
    -- | This is no a true instruction, but rather a combination of instructions. This whould
    -- perform the first `Instruction` and then the second.
    --
    -- This is used as `.` in the `Category` instance
    InstrConcat ::
        Instruction wt as bs ->
        Instruction wt bs cs ->
        Instruction wt as cs
    -- | Perform no actions. This is `id` in the `Category` instance
    InstrNOP ::
        Instruction wt xs xs
    -- | Drop the head of the stack
    InstrDrop ::
        Instruction wt (x ': xs) xs
    -- | Push a constant onto the stack
    InstrConst ::
        SNumericType t ->
        NumericVal t ->
        Instruction wt xs (t ': xs)
    -- | Add the top two elements on the stack
    InstrAdd ::
        SNumericType t ->
        Instruction wt (t ': t ': xs) (t ': xs)
    -- | Multiply the top two elements on the stack
    InstrMul ::
        SNumericType t ->
        Instruction wt (t ': t ': xs) (t ': xs)
    -- | Subtract the second element of the stack from the first
    InstrSub ::
        SNumericType t ->
        Instruction wt (t ': t ': xs) (t ': xs)
    InstrSqrt :: SFloatNumericType t -> Instruction wt (t ': xs) (t ': xs)
    -- | Read a global reference
    InstrGlobalGet ::
        TargetReference wt m t ->
        Instruction wt xs (t ': xs)
    -- | Alter a global reference
    InstrGlobalSet ::
        TargetReference wt 'Mutable t ->
        Instruction wt (t ': xs) xs
    -- | Call a function reference
    InstrCallFunc ::
        (SingI is, SingI o) =>
        Sing xs ->
        TargetFunction wt is o ->
        Instruction wt (ConcatList is xs) (PrependMaybe o xs)
    InstrIf ::
        Block wt '[] o ->
        Block wt '[] o ->
        Instruction wt ('I32 ': is) (PrependMaybe o is)
    -- | Jump to this target
    InstrJump :: TargetJumpTarget wt is os -> Instruction wt is os
    -- | Set up a new jump target.
    --
    -- This will run the `Instruction` created by the continuation. When ever one jumps to the
    -- given jump target this `Instruction` will restart
    InstrLoop :: (TargetJumpTarget wt '[] '[] -> Instruction wt '[] '[]) -> Instruction wt xs xs
    -- | Load the memory at this address.
    --
    -- The number of bytes read will depend on the the type of the read value
    InstrMemoryLoad :: Sing t -> Instruction wt ('I32 ': xs) (t ': xs)
    -- | Store the value at the `I32` index in memory.
    --
    -- The number of bytes read will depend on the the type of the stored value
    InstrMemoryStore :: Sing t -> Instruction wt ('I32 ': t ': xs) xs

instance Category (Instruction wt) where
    id = InstrNOP
    (.) = flip InstrConcat

instance Semigroup (Instruction wt as as) where
    (<>) = flip (.)

instance Monoid (Instruction wt as as) where
    mempty = id

instrConst :: (SingI t) => NumericVal t -> Instruction wt xs (t ': xs)
instrConst = InstrConst sing

instrAdd :: (SingI t) => Instruction wt (t ': t ': xs) (t ': xs)
instrAdd = InstrAdd sing

instrIfReturn ::
    Block wt '[] ('Just o) ->
    Block wt '[] ('Just o) ->
    Instruction wt ('I32 ': is) (o ': is)
instrIfReturn = InstrIf

instrIfNoReturn ::
    Block wt '[] 'Nothing ->
    Block wt '[] 'Nothing ->
    Instruction wt ('I32 ': is) is
instrIfNoReturn = InstrIf

instrMemoryLoad :: (SingI t) => Instruction wt ('I32 ': xs) (t ': xs)
instrMemoryLoad = InstrMemoryLoad sing

instrMemoryStore :: (SingI t) => Instruction wt ('I32 ': t ': xs) xs
instrMemoryStore = InstrMemoryStore sing

{- | Construct a type save function definition.

For an explanation of the type parameters see `Instruction`
-}
data
    FunctionDefinition
        (wt :: Type)
        (is :: [NumericType])
        (o :: Maybe NumericType)
    where
    -- | Augment a `FunctionDefinition` with a local parameter. Not that this does
    -- not alter the input type
    FDWithLocal ::
        SNumericType t ->
        (TargetReference wt m t -> FunctionDefinition wt is os) ->
        FunctionDefinition wt is os
    -- | Augment a `FunctionDefinition` with a local parameter that takes from the stack
    -- when called
    FDWithParam ::
        SNumericType t ->
        (TargetReference wt m t -> FunctionDefinition wt is os) ->
        FunctionDefinition wt (t ': is) os
    FDBody :: Block wt '[] o -> FunctionDefinition wt '[] o

fdWithParam ::
    (SingI t) =>
    (TargetReference wt m t -> FunctionDefinition wt is os) ->
    FunctionDefinition wt (t ': is) os
fdWithParam = FDWithParam sing

fdWithLocal ::
    (SingI t) =>
    (TargetReference wt m t -> FunctionDefinition wt is os) ->
    FunctionDefinition wt is os
fdWithLocal = FDWithLocal sing

fdResultBody ::
    (SingI o) =>
    Instruction wt '[] '[o] ->
    FunctionDefinition wt '[] ('Just o)
fdResultBody = FDBody . BlockSingleReturn sing

fdNoResultBody :: Instruction wt '[] '[] -> FunctionDefinition wt '[] Nothing
fdNoResultBody = FDBody . BlockNoReturn

{- | Define the memory section of a WASM module.

One option is `NoMemory`, to indicate that you don't want to use any memory.

The other is to indicate the memory with a minimum number of pages and a maximum
-}
data Memory = NoMemory | Memory Int32 Int32
    deriving stock (Eq, Show, Ord)

{- | A WASM module.

This can be quite difficult to work with. One should prefer `ModuleBuilder`, which provides
a monadic interface to creating thes values
-}
data Module (wt :: Type) where
    ModuleBase :: Memory -> Module wt
    ModuleFunction :: FunctionDefinition wt is o -> (TargetFunction wt is o -> Module wt) -> Module wt
    ModuleGlobal :: (SingI t) => SMutability m -> NumericVal t -> (TargetReference wt m t -> Module wt) -> Module wt
    ModuleExportFunc :: Text -> TargetFunction wt is o -> Module wt -> Module wt
    ModuleAddData :: Int32 -> ByteString -> Module wt -> Module wt

{- | A conviniet way to construct `Module`s.

Rather that building a `Module` manually, one should prefer to construct a `ModuleBuilder` and
call `buildModule`
-}
newtype ModuleBuilder (wt :: Type) (a :: Type) = ModuleBuilder (Cont (Module wt) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Create a `Module` from a `ModuleBuilder`
buildModule :: ModuleBuilder wt Memory -> Module wt
buildModule (ModuleBuilder c) = runCont c ModuleBase

-- | Add a function to the module and get a reference to it
moduleFunction :: FunctionDefinition wt is o -> ModuleBuilder wt (TargetFunction wt is o)
moduleFunction fd = ModuleBuilder $ cont (ModuleFunction fd)

moduleGlobal ::
    (SingI m, SingI t) =>
    NumericVal t ->
    ModuleBuilder wt (TargetReference wt m t)
moduleGlobal nv = ModuleBuilder $ cont $ ModuleGlobal sing nv

moduleExportFunc :: Text -> TargetFunction wt is o -> ModuleBuilder wt ()
moduleExportFunc name fr = ModuleBuilder $ cont (\f -> ModuleExportFunc name fr $ f ())

-- | Add this data to the memory at the given offset
moduleAddData :: Int32 -> ByteString -> ModuleBuilder wt ()
moduleAddData offset d = ModuleBuilder $ cont $ \f -> ModuleAddData offset d $ f ()
