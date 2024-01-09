module TypedWasm.Test.Example where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import Shelly hiding ((<.>), (</>))
import System.FilePath ((<.>), (</>))
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.List
import TypedWasm.Definition.Module
import TypedWasm.Definition.Types
import TypedWasm.WAT.Export (convertModule)

showConstantRep :: ConstantRep i -> Text
showConstantRep (CRI32 n) = T.pack $ show n
showConstantRep (CRI64 n) = T.pack $ show n
showConstantRep (CRF32 n) = T.pack $ show n
showConstantRep (CRF64 n) = T.pack $ show n

readConstantRep :: SNumericType t -> Text -> ConstantRep t
readConstantRep SNI32 t = CRI32 $ read $ T.unpack t
readConstantRep SNI64 t = CRI64 $ read $ T.unpack t
readConstantRep SNF32 t = CRF32 $ read $ T.unpack t
readConstantRep SNF64 t = CRF64 $ read $ T.unpack t

{- | Compile a WASM function into a wasm file, and provide a function
to call this function.

This will have a memory with a minimum size of 0 and a maximum size of 1
-}
compileSingleFunction ::
    forall o is a.
    (SingValueType o, SingNumericType o) =>
    (forall wt. FunctionDef wt is '[o]) ->
    ((HList ConstantRep is -> Sh (ConstantRep o)) -> Sh a) ->
    IO a
compileSingleFunction fd k = shelly $ silently $ withTmpDir $ \workingDir -> do
    let m = runModuleBuilder $ do
            addFunction fd >>= exportFunction "_start"
            return $ Memory 0 1
    liftIO
        $ T.writeFile (workingDir </> "module" <.> "wat")
        $ renderStrict
        $ layoutPretty defaultLayoutOptions
        $ pretty
        $ convertModule m
    run_
        "wat2wasm"
        [ T.pack (workingDir </> "module" <.> "wat")
        , "-o"
        , T.pack (workingDir </> "module" <.> "wasm")
        ]
    k $ \inputs -> do
        result <-
            run
                "wasmer"
                ( ["run", T.pack (workingDir </> "module" <.> "wasm"), "--"]
                    <> foldHList (\cr -> [showConstantRep cr]) inputs
                )
        pure $ readConstantRep singNumericType result
