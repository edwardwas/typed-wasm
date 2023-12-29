module TypedWasm.Instruction where

import Control.Category
import Control.Monad.Cont
import Data.Kind (Type)
import Data.Singletons
import Data.Text (Text)
import TypedWasm.Numeric
import Prelude hiding (id, (.))

type FuncKind = [NumericType] -> Maybe NumericType -> Type

type RefKind = Mutability -> NumericType -> Type

-- | A type family for combining lists
type family ConcatList as bs where
    ConcatList '[] bs = bs
    ConcatList (a ': as) bs = a ': ConcatList as bs

-- | If @ma@ is `Just`, prepend it to @bs@. Otherwise this is just @bs@
type family PrependMaybe ma bs where
    PrependMaybe 'Nothing bs = bs
    PrependMaybe ('Just a) bs = a ': bs

data Block rf g is o where
    BlockNoReturn :: Instruction rf g is '[] -> Block rf g is 'Nothing
    BlockSingleReturn :: Sing o -> Instruction rf g is '[o] -> Block rf g is ('Just o)

blockSingleReturn :: (SingI o) => Instruction rf g is '[o] -> Block rf g is ('Just o)
blockSingleReturn = BlockSingleReturn sing

{- | A block of WASM instructions.

This has may type variables. They are as follows:

* @fr@ - The types of wasm functions. Functions cannot be defined in instructions,
they must be defined at the top level. Hence there are now ways to produce @fr@s here,
just ways to consume them.
* @g@ - The types of top level references. Like functions, they cannot be defined here,
only consumed.
* @is@ - A type level list describing the stack before this operation
* @os@ - A type level list describing the stack after this operation.

There are two subtle points around polymorphism to keep in mind here.

Firstly, While constructing `Instructions` one should keep @fr@ and @g@ as
polymorphic as possible. Functions that consume `Instructions` will instantiate
them as required.

Secondly, both @is@ and @os@ should be written with a polymorphic tail. This
allows them to be used in more places
-}
data
    Instruction
        (fr :: FuncKind)
        (g :: RefKind)
        (is :: [NumericType])
        (os :: [NumericType])
    where
    -- | This is no a true instruction, but rather a combination of instructions. This whould
    -- perform the first `Instruction` and then the second.
    --
    -- This is used as `.` in the `Category` instance
    InstrConcat ::
        Instruction fr g as bs ->
        Instruction fr g bs cs ->
        Instruction fr g as cs
    -- | Perform no actions. This is `id` in the `Category` instance
    InstrNOP ::
        Instruction fr g xs xs
    -- | Drop the head of the stack
    InstrDrop ::
        Instruction fr g (x ': xs) xs
    -- | Push a constant onto the stack
    InstrConst ::
        SNumericType t ->
        NumericVal t ->
        Instruction fr g xs (t ': xs)
    -- | Add the top two elements on the stack
    InstrAdd ::
        SNumericType t ->
        Instruction fr g (t ': t ': xs) (t ': xs)
    -- | Multiply the top two elements on the stack
    InstrMul ::
        SNumericType t ->
        Instruction fr g (t ': t ': xs) (t ': xs)
    -- | Subtract the second element of the stack from the first
    InstrSub ::
        SNumericType t ->
        Instruction fr g (t ': t ': xs) (t ': xs)
    -- | Read a global reference
    InstrGlobalGet ::
        g m t ->
        Instruction fr g xs (t ': xs)
    -- | Alter a global reference
    InstrGlobalSet ::
        g 'Mutable t ->
        Instruction fr g (t ': xs) xs
    -- | Call a function reference
    InstrCallFunc ::
        (SingI is, SingI o) =>
        Sing xs ->
        fr is o ->
        Instruction fr g (ConcatList is xs) (PrependMaybe o xs)
    InstrIf ::
        Sing os ->
        Block fr g '[] o ->
        Block fr g '[] o ->
        Instruction fr g ('I32 ': is) (PrependMaybe o os)

instance Category (Instruction fr global) where
    id = InstrNOP
    (.) = flip InstrConcat

instrConst :: (SingI t) => NumericVal t -> Instruction f g xs (t ': xs)
instrConst = InstrConst sing

instrAdd :: (SingI t) => Instruction f g (t ': t ': xs) (t ': xs)
instrAdd = InstrAdd sing

instrIfReturn ::
    (SingI os) =>
    Block rf g '[] ('Just o) ->
    Block rf g '[] ('Just o) ->
    Instruction rf g ('I32 ': is) (o ': os)
instrIfReturn = InstrIf sing

instrIfNoReturn ::
    (SingI os) =>
    Block rf g '[] 'Nothing ->
    Block rf g '[] 'Nothing ->
    Instruction rf g ('I32 ': is) os
instrIfNoReturn = InstrIf sing

{- | Construct a type save function definition.

For an explanation of the type parameters see `Instruction`
-}
data
    FunctionDefinition
        (f :: FuncKind)
        (r :: RefKind)
        (is :: [NumericType])
        (o :: Maybe NumericType)
    where
    -- | Augment a `FunctionDefinition` with a local parameter. Not that this does
    -- not alter the input type
    FDWithLocal ::
        SNumericType t ->
        (r m t -> FunctionDefinition f r is os) ->
        FunctionDefinition f r is os
    -- | Augment a `FunctionDefinition` with a local parameter that takes from the stack
    -- when called
    FDWithParam ::
        SNumericType t ->
        (r m t -> FunctionDefinition f r is os) ->
        FunctionDefinition f r (t ': is) os
    FDBody :: Block f r '[] o -> FunctionDefinition f r '[] o

fdWithParam ::
    (SingI t) =>
    (r m t -> FunctionDefinition f r is os) ->
    FunctionDefinition f r (t ': is) os
fdWithParam = FDWithParam sing

fdWithLocal ::
    (SingI t) =>
    (r m t -> FunctionDefinition f r is os) ->
    FunctionDefinition f r is os
fdWithLocal = FDWithLocal sing

fdResultBody ::
    (SingI o) =>
    Instruction f r '[] '[o] ->
    FunctionDefinition f r '[] ('Just o)
fdResultBody = FDBody . BlockSingleReturn sing

{- | A WASM module.

This can be quite difficult to work with. One should prefer `ModuleBuilder`, which provides
a monadic interface to creating thes values
-}
data Module (f :: FuncKind) (r :: RefKind) where
    ModuleBase :: Module f r
    ModuleFunction :: FunctionDefinition f r is o -> (f is o -> Module f r) -> Module f r
    ModuleGlobal :: (SingI t) => SMutability m -> NumericVal t -> (r m t -> Module f r) -> Module f r
    ModuleExportFunc :: Text -> f is o -> Module f r -> Module f r

{- | A conviniet way to construct `Module`s.

Rather that building a `Module` manually, one should prefer to construct a `ModuleBuilder` and
call `buildModule`
-}
newtype ModuleBuilder (f :: FuncKind) (r :: RefKind) (a :: Type) = ModuleBuilder (Cont (Module f r) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Create a `Module` from a `ModuleBuilder`
buildModule :: ModuleBuilder f r a -> Module f r
buildModule (ModuleBuilder c) = runCont c (const ModuleBase)

-- | Add a function to the module and get a reference to it
moduleFunction :: FunctionDefinition f r is o -> ModuleBuilder f r (f is o)
moduleFunction fd = ModuleBuilder $ cont (ModuleFunction fd)

moduleGlobal ::
    (SingI m, SingI t) =>
    NumericVal t ->
    ModuleBuilder f r (r m t)
moduleGlobal nv = ModuleBuilder $ cont $ ModuleGlobal sing nv

moduleExportFunc :: Text -> f is o -> ModuleBuilder f r ()
moduleExportFunc name fr = ModuleBuilder $ cont (\f -> ModuleExportFunc name fr $ f ())
