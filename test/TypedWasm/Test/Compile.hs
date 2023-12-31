{-# LANGUAGE RecordWildCards #-}

module TypedWasm.Test.Compile where

import Control.Category ((>>>))
import Data.Text qualified as T
import Prelude.Singletons (sing)
import Shelly
import Test.Tasty
import Test.Tasty.HUnit
import TypedWasm.Instruction
import TypedWasm.Numeric
import TypedWasm.SExpr (moduleToWatFile)

data ModuleTest = ModuleTest
    { moduleTestName :: String
    , moduleTestModule :: forall wt. ModuleBuilder wt Memory
    }

singleModuleTest :: String -> (forall wt. FunctionDefinition wt '[I32] ('Just 'I32)) -> ModuleTest
singleModuleTest name funcDef = ModuleTest name $ do
    funcRef <- moduleFunction funcDef
    moduleExportFunc "_start" funcRef
    return NoMemory

moduleTestTree :: ModuleTest -> TestTree
moduleTestTree ModuleTest{..} =
    testCase moduleTestName $ do
        shelly $ withTmpDir $ \runningDir -> do
            mkdir_p "testWatFiles"
            liftIO
                $ moduleToWatFile
                    (buildModule moduleTestModule)
                    (runningDir </> ("module.wat" :: String))
            liftIO
                $ moduleToWatFile
                    (buildModule moduleTestModule)
                    ("testWatFiles/" <> moduleTestName <> ".wat")
            _ <-
                run
                    "wat2wasm"
                    [T.pack (runningDir <> "/module.wat")]
            return ()

allTests :: TestTree
allTests =
    testGroup "Module tests"
        $ map
            moduleTestTree
            [ ModuleTest "Single constant" $ do
                _ <- moduleFunction $ fdResultBody $ instrConst $ NVI32 0
                return NoMemory
            , ModuleTest "If single return with false" $ do
                _ <-
                    moduleFunction
                        $ fdResultBody
                            ( instrConst (NVI32 0)
                                >>> instrIfReturn
                                    (blockSingleReturn $ instrConst $ NVI32 100)
                                    (blockSingleReturn $ instrConst $ NVI32 200)
                            )
                return NoMemory
            , ModuleTest "Simple data section" $ do
                moduleAddData 0 "hello world"
                return $ Memory 1 1
            , singleModuleTest "Trigger different operations with args"
                $ fdWithParam
                $ \arg ->
                    fdWithLocal $ \a ->
                        fdWithLocal $ \b ->
                            fdResultBody
                                ( instrConst (NVI32 3)
                                    >>> InstrGlobalSet a
                                    >>> instrConst (NVI32 2)
                                    >>> InstrGlobalSet b
                                    >>> InstrGlobalGet arg
                                    >>> instrIfReturn
                                        ( blockSingleReturn
                                            ( InstrGlobalGet a
                                                >>> InstrGlobalGet b
                                                >>> instrAdd
                                            )
                                        )
                                        ( blockSingleReturn
                                            ( InstrGlobalGet a
                                                >>> InstrGlobalGet b
                                                >>> InstrMul sing
                                            )
                                        )
                                )
            , singleModuleTest "Loop with no jump"
                $ fdWithParam @I32
                $ \arg ->
                    fdResultBody
                        ( InstrLoop (\_ -> instrConst (NVI64 10) >>> InstrDrop)
                            >>> InstrGlobalGet arg
                        )
            , singleModuleTest
                "Loop with jump"
                $ fdWithParam @I32
                $ \arg ->
                    fdWithLocal @I32 $ \result ->
                        fdWithLocal @I32 $ \loopCounter ->
                            fdResultBody
                                ( instrConst 10
                                    >>> InstrGlobalSet loopCounter
                                    >>> instrConst 0
                                    >>> InstrGlobalSet result
                                    >>> InstrLoop
                                        ( \j ->
                                            InstrGlobalGet loopCounter
                                                >>> instrIfNoReturn
                                                    ( BlockNoReturn
                                                        ( InstrGlobalGet arg
                                                            >>> InstrGlobalGet result
                                                            >>> instrAdd
                                                            >>> InstrGlobalSet result
                                                            >>> InstrGlobalGet loopCounter
                                                            >>> instrConst (-1)
                                                            >>> instrAdd
                                                            >>> InstrGlobalSet loopCounter
                                                            >>> InstrJump j
                                                        )
                                                    )
                                                    (BlockNoReturn InstrNOP)
                                        )
                                    >>> InstrGlobalGet result
                                )
            ]
