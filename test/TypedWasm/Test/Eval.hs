module TypedWasm.Test.Eval where

import Data.Int (Int32)
import Test.Tasty
import Test.Tasty.HUnit
import TypedWasm.Instruction

createEvalTestCase :: String -> (forall f r. Instruction f r '[] '[I32]) -> Int32 -> TestTree
createEvalTestCase name instructions expected =
    let actual = undefined
     in testCase name undefined
