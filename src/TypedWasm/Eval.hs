module TypedWasm.Eval where

import Control.Monad.ST
import Data.Kind (Type)
import Data.List.Singletons
import Data.STRef
import Data.Singletons
import TypedWasm.Instruction
import TypedWasm.Numeric

data ValueStack (xs :: [NumericType]) where
    EmptyValueStack :: ValueStack '[]
    ConsValueStack :: NumericVal n -> ValueStack ns -> ValueStack (n ': ns)

deriving instance Show (ValueStack xs)
deriving instance Eq (ValueStack xs)

splitValueStack :: Sing as -> ValueStack (ConcatList as bs) -> (ValueStack as, ValueStack bs)
splitValueStack SNil vs = (EmptyValueStack, vs)
splitValueStack (SCons _ s) (ConsValueStack v vs) =
    let (as, bs) = splitValueStack s vs
     in (ConsValueStack v as, bs)

appendValueStack :: ValueStack as -> ValueStack bs -> ValueStack (ConcatList as bs)
appendValueStack EmptyValueStack vs = vs
appendValueStack (ConsValueStack a as) bs = ConsValueStack a $ appendValueStack as bs

applyBinaryFuncToValueStack ::
    (NumericVal a -> NumericVal b -> NumericVal c) ->
    ValueStack (a ': b ': cs) ->
    ValueStack (c ': cs)
applyBinaryFuncToValueStack f (ConsValueStack a (ConsValueStack b c)) = ConsValueStack (f a b) c

data EvalFunc (st :: Type) (is :: [NumericType]) (o :: Maybe NumericType) where
    EFSingleReturn :: (ValueStack is -> ST st (NumericVal n)) -> EvalFunc st is ('Just n)
    EFNoReturnValue :: (ValueStack is -> ST st ()) -> EvalFunc st is 'Nothing

newtype EvalReference st (m :: Mutability) (t :: NumericType) = EvalReference (STRef st (NumericVal t))

newtype EvalJump st (is :: [NumericType]) (os :: [NumericType])
    = EvalJump
        (ValueStack is -> ST st (ValueStack os))

data EvalTarget (st :: Type)

instance WasmTarget (EvalTarget st) where
    type TargetFunction (EvalTarget st) = EvalFunc st
    type TargetReference (EvalTarget st) = EvalReference st
    type TargetJumpTarget (EvalTarget st) = EvalJump st

evaluateInstruction ::
    forall st is os.
    Instruction (EvalTarget st) is os ->
    ValueStack is ->
    ST st (ValueStack os)
evaluateInstruction (InstrConcat a b) s =
    evaluateInstruction a s >>= evaluateInstruction b
evaluateInstruction InstrNOP s = pure s
evaluateInstruction InstrDrop (ConsValueStack _ s) = pure s
evaluateInstruction (InstrConst _ v) s = pure $ ConsValueStack v s
evaluateInstruction (InstrAdd si) s = withSingI si $ pure $ applyBinaryFuncToValueStack (+) s
evaluateInstruction (InstrMul si) s = withSingI si $ pure $ applyBinaryFuncToValueStack (*) s
evaluateInstruction (InstrSub si) s = withSingI si $ pure $ applyBinaryFuncToValueStack (-) s
evaluateInstruction (InstrGlobalGet (EvalReference r)) s = (`ConsValueStack` s) <$> readSTRef r
evaluateInstruction (InstrGlobalSet (EvalReference r)) (ConsValueStack a s) = s <$ writeSTRef r a
evaluateInstruction (InstrCallFunc _ ef) stack = case ef of
    EFSingleReturn func -> do
        let (as, bs) = splitValueStack sing stack
        nv <- func as
        return $ ConsValueStack nv bs
    EFNoReturnValue func -> do
        let (as, bs) = splitValueStack sing stack
        func as
        return bs
evaluateInstruction
    (InstrIf trueBranch falseBranch)
    (ConsValueStack (NVI32 check) stack) = case branch of
        BlockNoReturn instr -> stack <$ evaluateInstruction instr EmptyValueStack
        BlockSingleReturn _ instr ->
            evaluateInstruction instr EmptyValueStack >>= \case
                ConsValueStack nv EmptyValueStack -> pure $ ConsValueStack nv stack
      where
        branch = if check == 0 then falseBranch else trueBranch
evaluateInstruction (InstrJump (EvalJump f)) stack = f stack
evaluateInstruction (InstrLoop k) stack = do
    _ :: ValueStack '[] <-
        evaluateInstruction
            (k $ EvalJump (evaluateInstruction (InstrLoop k)))
            EmptyValueStack
    pure stack
