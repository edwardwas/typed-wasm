module TypedWasm.Test.Example where

import Control.Monad (forM_)
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.List
import TypedWasm.Definition.Module
import TypedWasm.Definition.Types
import TypedWasm.WAT.Export (convertModule)

showConstantRep :: ConstantRep i -> String
showConstantRep (CRI32 n) = show n
showConstantRep (CRI64 n) = show n
showConstantRep (CRF32 n) = show n
showConstantRep (CRF64 n) = show n

readConstantRep :: SNumericType t -> String -> ConstantRep t
readConstantRep SNI32 = CRI32 . read
readConstantRep SNI64 = CRI64 . read
readConstantRep SNF32 = CRF32 . read
readConstantRep SNF64 = CRF64 . read

-- | Compile a module that exports a function and let it be callable from Haskell
compileModuleFunction ::
    (SingNumericType o) =>
    (forall wt. ModuleBuilder wt (TargetFunc wt is '[o], Memory)) ->
    ((HList ConstantRep is -> IO (ConstantRep o)) -> IO a) ->
    IO a
compileModuleFunction mb callback =
    let m = runModuleBuilder $ do
            (func, memory) <- mb
            exportFunction "_start" func
            return memory
     in withSystemTempDirectory "moduleTemp" $ \workingDir ->
            do
                T.writeFile (workingDir </> "module" <.> "wat")
                    $ renderStrict
                    $ layoutPretty defaultLayoutOptions
                    $ pretty
                    $ convertModule
                        m
                _ <-
                    createProcess
                        $ proc
                            "wat2wasm"
                            [ workingDir </> "module" <.> "wat"
                            , "-o"
                            , workingDir </> "module" <.> "wasm"
                            ]
                callback $ \inputs -> do
                    readConstantRep singNumericType
                        <$> readProcess
                            "wasmer"
                            ( ["run", workingDir </> "module" <.> "wasm", "--"]
                                <> foldHList (\cr -> [showConstantRep cr]) inputs
                            )
                            ""

-- | Create a `TestTree` that will run a wasm function against some known inputs
exampleTest ::
    forall is o.
    (SingNumericType o) =>
    TestName ->
    (forall wt. ModuleBuilder wt (TargetFunc wt is '[o], Memory)) ->
    [(HList ConstantRep is, ConstantRep o)] ->
    TestTree
exampleTest testName mb examples = testCaseSteps testName $ \step -> do
    step "Compiling.."
    compileModuleFunction mb $ \call -> forM_ examples $ \(is, o) -> do
        step (show is <> " -> " <> show o)
        o' <- call is
        o @=? o'

knownTests :: TestTree
knownTests =
    testGroup
        "Single tests"
        [ exampleTest @'[ 'I32] @I32
            "2 to the power"
            ( (,NoMemory)
                <$> addFunction
                    ( functionDef
                        $ \(HSingle soFar) (HSingle i) ->
                            1
                                >. InstrSetRef soFar
                                >. InstrLoop @'[] @'[]
                                    ( \j ->
                                        instrWhen
                                            (InstrGetRef i)
                                            ( mconcat
                                                [ (InstrGetRef i - 1) >. InstrSetRef i
                                                , (InstrGetRef soFar * 2) >. InstrSetRef soFar
                                                , InstrBreak j
                                                ]
                                            )
                                    )
                                >. InstrGetRef soFar
                    )
            )
            ( map
                (\(i, o) -> (HSingle (CRI32 i), CRI32 o))
                [(0, 1), (1, 2), (2, 4), (3, 8)]
            )
        ]
