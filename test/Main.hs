module Main (main) where

import Test.Tasty
import TypedWasm.Test.Compile qualified

main :: IO ()
main = defaultMain $ testGroup "Tests" [TypedWasm.Test.Compile.allTests]
