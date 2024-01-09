module Main (main) where

import Test.Tasty
import TypedWasm.Test.Ops

main :: IO ()
main =
    defaultMain
        $ testGroup
            "Test"
            [ testGroup
                "Operation test"
                [ integralUnaryTests
                , integralBinaryTests
                ]
            ]
