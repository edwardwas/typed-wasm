module TypedWasm.WAT.Export where

import Data.Generics.Labels ()
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prelude.Singletons
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.List
import TypedWasm.Definition.Memory
import TypedWasm.Definition.Module
import TypedWasm.Definition.Types
import TypedWasm.WAT.SExpr

numericTypeFramgnet :: SNumericType t -> Text
numericTypeFramgnet SNI32 = "i32"
numericTypeFramgnet SNI64 = "i64"
numericTypeFramgnet SNF32 = "f32"
numericTypeFramgnet SNF64 = "f64"

renderValueType :: ValueType -> Text
renderValueType I32 = "i32"
renderValueType F32 = "f32"
renderValueType I64 = "i64"
renderValueType F64 = "f64"
renderValueType ExternRef = undefined
renderValueType Func = undefined

constantRepFragment :: ConstantRep t -> Text
constantRepFragment (CRI32 n) = T.pack $ show n
constantRepFragment (CRI64 n) = T.pack $ show n
constantRepFragment (CRF32 n) = T.pack $ show n
constantRepFragment (CRF64 n) = T.pack $ show n

integralUnaryOpFragment :: IntegralUnaryOp -> Text
integralUnaryOpFragment IUOCountLeadingZeros = "clz"
integralUnaryOpFragment IUOCountTrailingZeros = "ctz"
integralUnaryOpFragment IUOPopulationCount = "popcnt"

signedFragment :: Signed -> Text
signedFragment Signed = "_s"
signedFragment Unsigned = "_u"

integralBinaryOpFragment :: IntegralBinaryOp -> Text
integralBinaryOpFragment IBOAdd = "add"
integralBinaryOpFragment IBOMul = "mul"
integralBinaryOpFragment IBOSub = "sub"
integralBinaryOpFragment (IBODiv s) = "div" <> signedFragment s
integralBinaryOpFragment (IBORem s) = "rem" <> signedFragment s
integralBinaryOpFragment IBOAnd = "and"
integralBinaryOpFragment IBOOr = "or"
integralBinaryOpFragment IBOXor = "xor"
integralBinaryOpFragment IBOShiftLeft = "shl"
integralBinaryOpFragment (IBOShiftRight s) = "shr" <> signedFragment s
integralBinaryOpFragment IBORotateLeft = "rotl"
integralBinaryOpFragment IBORotateRight = "rotr"

floatingUnaryOpFragment :: FloatingUnaryOp -> Text
floatingUnaryOpFragment FUOAbs = "abs"
floatingUnaryOpFragment FUONeg = "neg"
floatingUnaryOpFragment FUOSqrt = "sqrt"
floatingUnaryOpFragment FUOCeil = "ceil"
floatingUnaryOpFragment FUOFloor = "floor"
floatingUnaryOpFragment FUOTruncate = "trunc"
floatingUnaryOpFragment FUONearest = "nearest"

floatingBinaryOpFragment :: FloatingBinaryOp -> Text
floatingBinaryOpFragment FBOAdd = "add"
floatingBinaryOpFragment FBOSub = "sub"
floatingBinaryOpFragment FBOMul = "mul"
floatingBinaryOpFragment FBODiv = "div"
floatingBinaryOpFragment FBOMin = "min"
floatingBinaryOpFragment FBOMax = "max"
floatingBinaryOpFragment FBOCopySign = "copysign"

renderFuncType :: HList SNumericType is -> HList SNumericType os -> [SExpr]
renderFuncType HEmpty HEmpty = []
renderFuncType HEmpty (HCons s HEmpty) = [atomList ["result", numericTypeFramgnet s]]
renderFuncType _ _ = error "Need more func types"

data LocalOrGlobal = Local | Global

data SExprTarget

instance WasmTarget SExprTarget where
    newtype TargetJumpLabel SExprTarget vt = SExprTargetJumpLabel Int
    newtype TargetFunc SExprTarget is os = SExprTargetFunc Int
    data TargetRef SExprTarget m t = SExprTargetRef LocalOrGlobal Int

type JumpDepth = Int

convertInstruction :: forall is os. JumpDepth -> Instruction SExprTarget is os -> [SExpr]
convertInstruction jd (InstrSequence a b) = convertInstruction jd a <> convertInstruction jd b
convertInstruction _ InstrNOP = ["nop"]
convertInstruction _ InstrUnreachable = ["unreachable"]
convertInstruction _ (InstrConst snt n) =
    [ atomList
        [ numericTypeFramgnet snt <> ".const"
        , constantRepFragment n
        ]
    ]
convertInstruction _ (InstrIntegralUnary sit op) =
    [ SExprAtom
        (numericTypeFramgnet (sIntegralTypeToNumeric sit) <> "." <> integralUnaryOpFragment op)
    ]
convertInstruction _ (InstrIntegralBinary sit op) =
    [ SExprAtom
        (numericTypeFramgnet (sIntegralTypeToNumeric sit) <> "." <> integralBinaryOpFragment op)
    ]
convertInstruction _ (InstrFloatingUnary sit op) =
    [ SExprAtom
        (numericTypeFramgnet (sFloatingTypeToNumeric sit) <> "." <> floatingUnaryOpFragment op)
    ]
convertInstruction _ (InstrFloatingBinary sit op) =
    [ SExprAtom
        (numericTypeFramgnet (sFloatingTypeToNumeric sit) <> "." <> floatingBinaryOpFragment op)
    ]
convertInstruction jd (InstrBreak (SExprTargetJumpLabel n)) =
    let targetNum = jd - n - 1
     in [atomList ["br", T.pack (show targetNum)]]
convertInstruction jd (InstrLoop k) =
    let childInstrs = convertInstruction (succ jd) (k $ SExprTargetJumpLabel 0)
        sis :: HList SNumericType is = singList (Proxy @SingNumericType) singNumericType
        sos :: HList SNumericType os = singList (Proxy @SingNumericType) singNumericType
        tyExprs = renderFuncType sis sos
     in [SExprList ("loop" : tyExprs <> childInstrs)]
convertInstruction jd (InstrIf (ifTrue :: Instruction wt is' os') ifFalse) =
    let trueInstrs = convertInstruction (succ jd) ifTrue
        falseInstrs = convertInstruction (succ jd) ifFalse
        sis :: HList SNumericType is' = singList (Proxy @SingNumericType) singNumericType
        sos :: HList SNumericType os' = singList (Proxy @SingNumericType) singNumericType
        tyExprs = renderFuncType sis sos
     in [ SExprList
            ( ("if" : tyExprs)
                <> [ SExprList ("then" : trueInstrs)
                   , SExprList ("else" : falseInstrs)
                   ]
            )
        ]
convertInstruction _ (InstrGetRef (SExprTargetRef Local n)) = [atomList ["local.get", T.pack (show n)]]
convertInstruction _ (InstrGetRef (SExprTargetRef Global n)) = [atomList ["global.get", T.pack (show n)]]
convertInstruction _ (InstrSetRef (SExprTargetRef Local n)) = [atomList ["local.set", T.pack (show n)]]
convertInstruction _ (InstrSetRef (SExprTargetRef Global n)) = [atomList ["global.set", T.pack (show n)]]

data FunctionTypeDefinition = FunctionTypeDefinition
    { ftdParams :: [ValueType]
    , ftdLocals :: [ValueType]
    }
    deriving stock (Eq, Show, Generic)

convertFunctionDef ::
    forall is os.
    (SingList SingValueType os) =>
    FunctionDef SExprTarget is os ->
    SExpr
convertFunctionDef (FunctionDef sLocals sArgs k) =
    let argTargetRefs = indexMapHList (\n _ -> SExprTargetRef Local n) sArgs
        argDefSExprs = foldHList (\sTy -> [atomList ["param", renderValueType $ lowerSValueType sTy]]) sArgs
        numArgs = countHList sArgs
        localTargetRefs = indexMapHList (\n _ -> SExprTargetRef Local (n + numArgs)) sLocals
        localDefSExprs = foldHList (\sTy -> [atomList ["local", renderValueType $ lowerSValueType sTy]]) sLocals
        bodyInstrs = k localTargetRefs argTargetRefs
        resultSExprs :: [SExpr] = case singValueTypeList @os of
            HEmpty -> []
            HCons o HEmpty -> [atomList ["result", renderValueType $ lowerSValueType o]]
            _ -> undefined
     in SExprList
            ( ["func"]
                <> argDefSExprs
                <> resultSExprs
                <> localDefSExprs
                <> convertInstruction 0 bodyInstrs
            )

convertModule :: ModuleDef SExprTarget -> SExpr
convertModule md = SExprList ("module" : helper 0 md)
  where
    helper :: Int -> ModuleDef SExprTarget -> [SExpr]
    helper _ (MDBase mem) = case mem of
        NoMemory -> []
        Memory minSize maxSize ->
            [ atomList
                [ "memory"
                , T.pack $ show minSize
                , T.pack $ show maxSize
                ]
            ]
    helper funcCount (MDAddFunction fd k) =
        convertFunctionDef fd
            : helper (succ funcCount) (k $ SExprTargetFunc funcCount)
    helper funcCount (MDExportFunction name (SExprTargetFunc n) next) =
        SExprList
            [ "export"
            , SExprAtom ("\"" <> name <> "\"")
            , atomList ["func", T.pack $ show n]
            ]
            : helper funcCount next
