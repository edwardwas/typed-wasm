module TypedWasm.Definition.Module where

import Control.Monad.Cont
import Data.Text (Text)
import Data.Word (Word32)
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.Types
import TypedWasm.Util.List

data FunctionDef wt is os where
    FunctionDef ::
        HList SValueType locals ->
        HList SValueType is ->
        ( HList (TargetRef wt 'Mutable) locals ->
          HList (TargetRef wt 'Mutable) is ->
          Instruction wt '[] os
        ) ->
        FunctionDef wt is os

functionDef ::
    forall locals is os wt.
    (SingList SingValueType locals, SingList SingValueType is) =>
    ( HList (TargetRef wt 'Mutable) locals ->
      HList (TargetRef wt 'Mutable) is ->
      Instruction wt '[] os
    ) ->
    FunctionDef wt is os
functionDef = FunctionDef singValueTypeList singValueTypeList

data Memory
    = NoMemory
    | Memory Word32 Word32
    deriving stock (Eq, Show)

data ModuleDef wt where
    MDBase :: Memory -> ModuleDef wt
    MDAddFunction ::
        (SingList SingValueType os) =>
        FunctionDef wt is os ->
        (TargetFunc wt is os -> ModuleDef wt) ->
        ModuleDef wt
    MDExportFunction :: Text -> TargetFunc wt is os -> ModuleDef wt -> ModuleDef wt

newtype ModuleBuilder wt a = ModuleBuilder (Cont (ModuleDef wt) a)
    deriving newtype (Functor, Applicative, Monad)

runModuleBuilder :: ModuleBuilder wt Memory -> ModuleDef wt
runModuleBuilder (ModuleBuilder act) = runCont act MDBase

addFunction ::
    (SingList SingValueType os) =>
    FunctionDef wt is os ->
    ModuleBuilder wt (TargetFunc wt is os)
addFunction fd = ModuleBuilder (cont $ MDAddFunction fd)

exportFunction :: Text -> TargetFunc wt is os -> ModuleBuilder wt ()
exportFunction name func = ModuleBuilder (cont $ \f -> MDExportFunction name func $ f ())
