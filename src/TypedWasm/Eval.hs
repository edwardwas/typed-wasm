module TypedWasm.Eval where

import Control.Category
import Control.Monad ((>=>))
import Control.Monad.Free
import Control.Monad.ST
import Data.List.Singletons
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Singletons
import TypedWasm.Instruction
import Prelude hiding (id, (.))

-- | A stack of numeric wasm values, indexed by the number of members and their types
data ValueStack (xs :: [NumericType]) where
    EmptyValueStack :: ValueStack '[]
    ConsValueStack ::
        NumericVal t ->
        ValueStack ts ->
        ValueStack (t ': ts)

deriving instance Eq (ValueStack t)
deriving instance Show (ValueStack t)

-- | Concat two `ValueStack`s. Note that this is checked at compile time with types
concatValueStack :: ValueStack as -> ValueStack bs -> ValueStack (ConcatList as bs)
concatValueStack EmptyValueStack bs = bs
concatValueStack (ConsValueStack a as) bs = ConsValueStack a $ concatValueStack as bs

-- | Split a value stack into a known substack and some unknown sub stack
splitValueStack :: SList as -> ValueStack (ConcatList as bs) -> (ValueStack as, ValueStack bs)
splitValueStack SNil bs = (EmptyValueStack, bs)
splitValueStack (SCons _ s) (ConsValueStack c cs) =
    let (as, bs) = splitValueStack s cs
     in (ConsValueStack c as, bs)

popValueStack :: ValueStack (x ': xs) -> (NumericVal x, ValueStack xs)
popValueStack (ConsValueStack x xs) = (x, xs)

{- | For global values, we use a `STRef`. @
m@ is a phantom type here - we don't treat mutable and immutable values differently
-}
newtype GlobalRef s (m :: Mutability) (t :: NumericType) = GlobalRef
    { getGlobalRef :: STRef s (NumericVal t)
    }
    deriving newtype (Eq)

{- | `Instruction`s are compiled to these "Interpret Functions". This is a function between `ValueStack`s,
with effects in an `ST` `Monad`
-}
newtype InterpretedFunction s (is :: [NumericType]) (os :: [NumericType]) = InterpretedFunction
    { runInterpretedFunction :: ValueStack is -> ST s (ValueStack os)
    }

instance Category (InterpretedFunction s) where
    id = InterpretedFunction pure
    InterpretedFunction a . InterpretedFunction b = InterpretedFunction (b >=> a)

{- | A helper function for implemented `evaluateInstruction`.

This lifts a function over two `NumericVal`s to an `InterpretedFunction`, modulo newtype wrappers
-}
binaryStackFunction ::
    (NumericVal a -> NumericVal b -> NumericVal c) ->
    ValueStack (a ': b ': xs) ->
    ST s (ValueStack (c ': xs))
binaryStackFunction f (ConsValueStack a (ConsValueStack b xs)) = pure $ ConsValueStack (f a b) xs

unaryStackFunction ::
    (NumericVal a -> NumericVal b) ->
    ValueStack (a ': xs) ->
    ST s (ValueStack (b ': xs))
unaryStackFunction f (ConsValueStack a as) = pure $ ConsValueStack (f a) as

-- | Evaluate an `Instruction` as a function from one `ValueStack` to another
evaluateInstruction ::
    Instruction (InterpretedFunction s) (GlobalRef s) is os ->
    ValueStack is ->
    ST s (ValueStack os)
evaluateInstruction (InstrConcat a b) s = evaluateInstruction a s >>= evaluateInstruction b
evaluateInstruction InstrNOP s = pure s
evaluateInstruction InstrDrop (ConsValueStack _ s) = pure s
evaluateInstruction (InstrConst _ x) s = pure $ ConsValueStack x s
evaluateInstruction (InstrAdd w) s = withSingI w $ binaryStackFunction (+) s
evaluateInstruction (InstrMul w) s = withSingI w $ binaryStackFunction (*) s
evaluateInstruction (InstrSub w) s = withSingI w $ binaryStackFunction (-) s
evaluateInstruction (InstrNegate w) s = withSingI w $ unaryStackFunction negate s
evaluateInstruction (InstrAbs w) s = withSingI w $ unaryStackFunction abs s
evaluateInstruction (InstrGlobalGet (GlobalRef var)) s = do
    v <- readSTRef var
    return $ ConsValueStack v s
evaluateInstruction (InstrGlobalSet (GlobalRef var)) (ConsValueStack v s) = s <$ writeSTRef var v
evaluateInstruction
    ( InstrCallFunc
            (_ :: Sing xs)
            (InterpretedFunction innerFunc :: InterpretedFunction s is' os')
        )
    stack = do
        let (as, bs :: ValueStack xs) = splitValueStack (sing @is') stack
        cs <- innerFunc as
        return $ concatValueStack cs bs
evaluateInstruction (InstrIf trueBranch falseBranch) s =
    let (x, xs) = popValueStack s
     in if x /= NVI32 0
            then evaluateInstruction trueBranch xs
            else evaluateInstruction falseBranch xs

evaluateFunctionDefinition ::
    FunctionDefinition (InterpretedFunction s) (GlobalRef s) is os ->
    ValueStack is ->
    ST s (ValueStack os)
evaluateFunctionDefinition (FDWithLocal k) s = do
    ref <- newSTRef 0
    evaluateFunctionDefinition (k $ GlobalRef ref) s
evaluateFunctionDefinition (FDWithParam k) (ConsValueStack x s) = do
    ref <- newSTRef x
    evaluateFunctionDefinition (k $ GlobalRef ref) s
evaluateFunctionDefinition (FDBody is) s = evaluateInstruction is s

-- | Evaluate a `ModuleBuilder` to an `ST` value
evaluateModuleBuilder :: ModuleBuilder (InterpretedFunction s) (GlobalRef s) x -> ST s x
evaluateModuleBuilder = iterM $ \case
    AddGlobal _ start cont -> newSTRef start >>= cont . GlobalRef
    DeclareFunc is cont -> cont $ InterpretedFunction (evaluateFunctionDefinition is)
    ExportFunction _ _ child -> child
