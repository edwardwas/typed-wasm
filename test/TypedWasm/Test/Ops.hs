module TypedWasm.Test.Ops where

import Data.Bits
import Test.Tasty
import TypedWasm.Definition.Constant
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.Memory
import TypedWasm.Definition.Module
import TypedWasm.Definition.Types
import TypedWasm.Test.Example
import TypedWasm.Util.Enum
import TypedWasm.Util.List

intergralUnaryExamples :: IntegralUnaryOp -> [(Int, Int)]
intergralUnaryExamples IUOCountLeadingZeros = []
intergralUnaryExamples IUOCountTrailingZeros = [(1, 0)]
intergralUnaryExamples IUOPopulationCount = [(0, 0), (1, 1), (2, 1)]

integralBinaryExamples :: IntegralBinaryOp -> [(Int, Int, Int)]
integralBinaryExamples IBOAdd = [(0, 0, 0), (2, 3, 5), (10, 3, 13), (10, -3, 7)]
integralBinaryExamples IBOSub = [(0, 0, 0), (3, 1, 2), (10, -5, 15), (0, 3, -3)]
integralBinaryExamples IBOMul =
    [ (0, 0, 0)
    , (0, 100, 0)
    , (1, 3, 3)
    , (3, 4, 12)
    , (-3, 4, -12)
    , (3, -4, -12)
    , (-3, -4, 12)
    ]
integralBinaryExamples (IBODiv s) =
    [(6, 2, 3), (6, 3, 2), (7, 3, 2)] <> case s of
        Signed -> [(6, -2, -3)]
        _ -> []
integralBinaryExamples (IBORem s) =
    [(6, 2, 0), (6, 3, 0), (7, 3, 1)] <> case s of
        Signed -> [(7, -2, 1)]
        _ -> []
integralBinaryExamples IBOAnd = [(0, 0, 0), (1, 0, 0), (0, 1, 0), (1, 1, 1)]
integralBinaryExamples IBOOr = [(0, 0, 0), (1, 0, 1), (0, 1, 1), (1, 1, 1)]
integralBinaryExamples IBOXor = [(0, 0, 0), (1, 0, 1), (0, 1, 1), (1, 1, 0)]
integralBinaryExamples IBOShiftLeft = map (\n -> (128, n, shift 128 n)) [0 .. 8]
integralBinaryExamples (IBOShiftRight _) = map (\n -> (128, n, shift 128 $ negate n)) [0 .. 8]
integralBinaryExamples IBORotateLeft = map (\n -> (128, n, rotate 128 n)) [0 .. 8]
integralBinaryExamples IBORotateRight = [(123, 0, 123)]

integralComparisonExamples :: IntegralComparison -> [(Int, Int, Bool)]
integralComparisonExamples c =
    let bf = case c of
            ICEqual -> (==)
            ICNotEqual -> (/=)
            ICLessThan _ -> (<)
            ICGreaterThan _ -> (>)
            ICLessThanOrEq _ -> (<=)
            ICGreaterThanOrEq _ -> (>=)
     in map (\(a, b) -> (a, b, bf a b)) [(0, 0), (1, 1), (123, 321), (321, 123)]

intToConstantRep :: SIntegralType t -> Int -> ConstantRep t
intToConstantRep SI32 = CRI32 . fromIntegral
intToConstantRep SI64 = CRI64 . fromIntegral

boolToI32 :: Bool -> ConstantRep 'I32
boolToI32 True = 1
boolToI32 False = 0

integralUnaryTests :: TestTree
integralUnaryTests =
    testGroup "Integral Unary Ops"
        $ map (\op -> testGroup (show op) [helper op SI32, helper op SI64]) allMembers
  where
    helper ::
        forall vt.
        (SingNumericType vt, SingValueType vt) =>
        IntegralUnaryOp ->
        SIntegralType vt ->
        TestTree
    helper iuo sTy =
        exampleTest
            (show sTy)
            ( (,NoMemory)
                <$> addFunction
                    ( functionDef @'[] $ \HEmpty (HSingle i) ->
                        InstrGetRef i >. InstrIntegralUnary sTy iuo
                    )
            )
            ( map (\(i, o) -> (HSingle (intToConstantRep sTy i), intToConstantRep sTy o))
                $ intergralUnaryExamples iuo
            )

integralBinaryTests :: TestTree
integralBinaryTests =
    testGroup "Integral Binary Ops"
        $ map (\op -> testGroup (show op) [helper op SI32, helper op SI64]) allMembers
  where
    helper ::
        forall vt.
        (SingNumericType vt, SingValueType vt) =>
        IntegralBinaryOp ->
        SIntegralType vt ->
        TestTree
    helper ibo sTy =
        exampleTest
            (show sTy)
            ( (,NoMemory)
                <$> addFunction
                    ( functionDef @'[] $ \HEmpty (a :* b) ->
                        InstrGetRef a >. InstrGetRef b >. InstrIntegralBinary sTy ibo
                    )
            )
            ( map
                ( \(a, b, o) ->
                    ( intToConstantRep sTy a :* intToConstantRep sTy b
                    , intToConstantRep sTy o
                    )
                )
                $ integralBinaryExamples ibo
            )

integralComparisonTests :: TestTree
integralComparisonTests =
    testGroup "Integral Comparisons"
        $ map
            ( \op ->
                testGroup
                    (show op)
                    [helper op SI32, helper op SI64]
            )
            allMembers
  where
    helper ::
        forall vt.
        (SingValueType vt, SingNumericType vt) =>
        IntegralComparison ->
        SIntegralType vt ->
        TestTree
    helper cop sTy =
        exampleTest
            (show sTy)
            ( (,NoMemory)
                <$> addFunction
                    ( functionDef @'[] @'[vt, vt] @'[ 'I32]
                        $ \HEmpty (a :* b) ->
                            InstrGetRef a
                                >. InstrGetRef b
                                >. InstrIntegralCompare sTy cop
                    )
            )
            ( map
                ( \(a, b, c) ->
                    ( intToConstantRep sTy a :* intToConstantRep sTy b
                    , boolToI32 c
                    )
                )
                $ integralComparisonExamples cop
            )

integralEqZeroTests :: TestTree
integralEqZeroTests = testGroup "Equal Zero" [helper SI32, helper SI64]
  where
    helper :: forall vt. (SingNumericType vt, SingValueType vt) => SIntegralType vt -> TestTree
    helper sty =
        exampleTest
            (show sty)
            ( (,NoMemory)
                <$> addFunction
                    ( functionDef @'[] @'[vt] @'[ 'I32]
                        $ \HEmpty (HSingle i) -> InstrGetRef i >. InstrEqualZero sty
                    )
            )
            [(HSingle 0, 1), (HSingle 1, 0), (HSingle 100, 0)]
