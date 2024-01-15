module TypedWasm.WAT where

import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import TypedWasm.Definition.Instruction
import TypedWasm.Definition.Memory
import TypedWasm.Definition.Module
import TypedWasm.Definition.Types
import TypedWasm.Util.List
import TypedWasm.Util.SExpr
import TypedWasm.Definition.Constant

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

integralComparisonFragment :: IntegralComparison -> Text
integralComparisonFragment ICEqual = "eq"
integralComparisonFragment ICNotEqual = "ne"
integralComparisonFragment (ICLessThan s) = "lt" <> signedFragment s
integralComparisonFragment (ICGreaterThan s) = "gt" <> signedFragment s
integralComparisonFragment (ICLessThanOrEq s) = "le" <> signedFragment s
integralComparisonFragment (ICGreaterThanOrEq s) = "ge" <> signedFragment s

floatingBinaryOpFragment :: FloatingBinaryOp -> Text
floatingBinaryOpFragment FBOAdd = "add"
floatingBinaryOpFragment FBOSub = "sub"
floatingBinaryOpFragment FBOMul = "mul"
floatingBinaryOpFragment FBODiv = "div"
floatingBinaryOpFragment FBOMin = "min"
floatingBinaryOpFragment FBOMax = "max"
floatingBinaryOpFragment FBOCopySign = "copysign"

floatingComparisonFragment :: FloatingComparison -> Text
floatingComparisonFragment FCEqual = "eq"
floatingComparisonFragment FCNotEqual = "ne"
floatingComparisonFragment FCLessThan = "lt"
floatingComparisonFragment FCGreaterThan = "gt"
floatingComparisonFragment FCLessThanOrEq = "le"
floatingComparisonFragment FCGreaterThanOrEq = "ge"

renderFuncType :: HList SNumericType is -> HList SNumericType os -> [SExpr]
renderFuncType HEmpty HEmpty = []
renderFuncType HEmpty (HCons s HEmpty) = [atomList ["result", numericTypeFramgnet s]]
renderFuncType _ _ = error "Need more func types"

convertMemoryStoreInstruction :: MemStoreInstruction vt -> Text
convertMemoryStoreInstruction (MSISelf sTy) = numericTypeFramgnet sTy <> ".store"
convertMemoryStoreInstruction (MSIInt8 sTy) = numericTypeFramgnet (sIntegralTypeToNumeric sTy) <> ".store8"
convertMemoryStoreInstruction (MSIInt16 sTy) = numericTypeFramgnet (sIntegralTypeToNumeric sTy) <> ".store16"
convertMemoryStoreInstruction MSIInt32 = "i64.store32"

convertMemoryLoadInstruction :: MemLoadInstruction vt -> Text
convertMemoryLoadInstruction (MLISelf sTy) = numericTypeFramgnet sTy <> ".load"
convertMemoryLoadInstruction (MLIInt8 sTy sign) =
    numericTypeFramgnet (sIntegralTypeToNumeric sTy)
        <> ".load8"
        <> signedFragment sign
convertMemoryLoadInstruction (MLIInt16 sTy sign) =
    numericTypeFramgnet (sIntegralTypeToNumeric sTy)
        <> ".load16"
        <> signedFragment sign
convertMemoryLoadInstruction (MLIInt32 sign) = "i64.load32" <> signedFragment sign

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
        ( numericTypeFramgnet (sIntegralTypeToNumeric sit)
            <> "."
            <> integralUnaryOpFragment op
        )
    ]
convertInstruction _ (InstrIntegralBinary sit op) =
    [ SExprAtom
        (numericTypeFramgnet (sIntegralTypeToNumeric sit) <> "." <> integralBinaryOpFragment op)
    ]
convertInstruction _ (InstrFloatingUnary sft op) =
    [ SExprAtom
        ( numericTypeFramgnet (sFloatingTypeToNumeric sft)
            <> "."
            <> floatingUnaryOpFragment op
        )
    ]
convertInstruction _ (InstrFloatingBinary sft op) =
    [ SExprAtom
        ( numericTypeFramgnet (sFloatingTypeToNumeric sft)
            <> "."
            <> floatingBinaryOpFragment op
        )
    ]
convertInstruction _ (InstrIntegralCompare sit cp) =
    [ SExprAtom
        ( numericTypeFramgnet (sIntegralTypeToNumeric sit)
            <> "."
            <> integralComparisonFragment cp
        )
    ]
convertInstruction _ (InstrFloatingCompare sft cp) =
    [ SExprAtom
        ( numericTypeFramgnet (sFloatingTypeToNumeric sft)
            <> "."
            <> floatingComparisonFragment cp
        )
    ]
convertInstruction _ (InstrEqualZero sit) =
    [ SExprAtom (numericTypeFramgnet (sIntegralTypeToNumeric sit) <> ".eqz")
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
convertInstruction _ (InstrLoadMemory mli) = [SExprAtom $ convertMemoryLoadInstruction mli]
convertInstruction _ (InstrStoreMemory mli) = [SExprAtom $ convertMemoryStoreInstruction mli]

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

-- | When converting modules, we need to keep track of some information as we walk the module.
data ModuleConversionInfo = ModuleConversionInfo
    { functionCount :: Int
    -- ^ The number of functions we've created
    , globalCount :: Int
    -- ^ The number of globals we've created
    }
    deriving (Eq, Show)

increaseFunctionCount :: ModuleConversionInfo -> ModuleConversionInfo
increaseFunctionCount mci = mci{functionCount = succ $ functionCount mci}

increaseGlobalCount :: ModuleConversionInfo -> ModuleConversionInfo
increaseGlobalCount mci = mci{globalCount = succ $ globalCount mci}

convertModule :: ModuleDef SExprTarget -> SExpr
convertModule md =
    SExprList
        ( "module"
            : helper
                ModuleConversionInfo
                    { functionCount = 0
                    , globalCount = 0
                    }
                md
        )
  where
    helper :: ModuleConversionInfo -> ModuleDef SExprTarget -> [SExpr]
    helper _ (MDBase mem) = case mem of
        NoMemory -> []
        Memory minSize maxSize ->
            [ atomList
                [ "memory"
                , T.pack $ show minSize
                , T.pack $ show maxSize
                ]
            ]
    helper mci (MDAddFunction fd k) =
        convertFunctionDef fd
            : helper
                (increaseFunctionCount mci)
                (k $ SExprTargetFunc $ functionCount mci)
    helper funcCount (MDExportFunction name (SExprTargetFunc n) next) =
        SExprList
            [ "export"
            , SExprAtom ("\"" <> name <> "\"")
            , atomList ["func", T.pack $ show n]
            ]
            : helper funcCount next
    helper mci (MDGlobalConstant cr k) =
        SExprList
            [ "global"
            , SExprAtom $ numericTypeFramgnet $ typeOfConstant cr
            , atomList
                [ numericTypeFramgnet (typeOfConstant cr) <> ".const"
                , constantRepFragment cr
                ]
            ]
            : helper
                (increaseGlobalCount mci)
                (k $ SExprTargetRef Global $ globalCount mci)
