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
    , moduleTestModule :: forall wt. ModuleBuilder wt ()
    }

moduleTestTree :: ModuleTest -> TestTree
moduleTestTree ModuleTest{..} =
    testCase moduleTestName $ do
        shelly $ withTmpDir $ \runningDir -> do
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
                return ()
            , ModuleTest "If single return with false" $ do
                _ <-
                    moduleFunction
                        $ fdResultBody
                            ( instrConst (NVI32 0)
                                >>> instrIfReturn
                                    (blockSingleReturn $ instrConst $ NVI32 100)
                                    (blockSingleReturn $ instrConst $ NVI32 200)
                            )
                return ()
            , ModuleTest "Trigger different operations with args" $ do
                _ <- moduleFunction $ fdWithParam $ \arg ->
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
                return ()
            ]
