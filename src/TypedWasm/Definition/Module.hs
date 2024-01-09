module TypedWasm.Definition.Module where

import Control.Monad.Cont
import Data.Text (Text)
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.List
import TypedWasm.Definition.Types

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
    (SingList SingValueType locals, SingList SingValueType is) =>
    ( HList (TargetRef wt 'Mutable) locals ->
      HList (TargetRef wt 'Mutable) is ->
      Instruction wt '[] os
    ) ->
    FunctionDef wt is os
functionDef = FunctionDef singValueTypeList singValueTypeList

data ModuleDef wt where
    MDBase :: ModuleDef wt
    MDAddFunction ::
        (SingList SingValueType os) =>
        FunctionDef wt is os ->
        (TargetFunc wt is os -> ModuleDef wt) ->
        ModuleDef wt
    MDExportFunction :: Text -> TargetFunc wt is os -> ModuleDef wt -> ModuleDef wt

newtype ModuleBuilder wt a = ModuleBuilder (Cont (ModuleDef wt) a)
    deriving newtype (Functor, Applicative, Monad)

runModuleBuilder :: ModuleBuilder wt a -> ModuleDef wt
runModuleBuilder (ModuleBuilder act) = runCont act (const MDBase)

addFunction ::
    (SingList SingValueType os) =>
    FunctionDef wt is os ->
    ModuleBuilder wt (TargetFunc wt is os)
addFunction fd = ModuleBuilder (cont $ MDAddFunction fd)

exportFunction :: Text -> TargetFunc wt is os -> ModuleBuilder wt ()
exportFunction name func = ModuleBuilder (cont $ \f -> MDExportFunction name func $ f ())
