module TypedWasm.SExpr where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Exts (IsString, fromString)
import Prelude.Singletons
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import TypedWasm.Instruction
import TypedWasm.Numeric

{- | WASM text format (wat) uses sexpressions to represent WASM. This is a haskell
encoding of an SExpression
-}
data SExpr
    = SExprAtom Text
    | SExprList [SExpr]
    deriving stock (Eq, Show)

instance IsString SExpr where
    fromString = SExprAtom . fromString

instance Pretty SExpr where
    pretty (SExprAtom t) = pretty t
    pretty (SExprList es) = parens $ group $ nest 1 $ vsep $ map pretty es

-- | Create a `SExpr` made from a list of atoms
atomList :: [Text] -> SExpr
atomList = SExprList . map SExprAtom

{- | References can either be global (at the top level), or local (only usable
in a given function). This tracks this
-}
data Locality = Global | Local
    deriving stock (Eq, Show, Ord)

renderLocality :: Locality -> Text
renderLocality Global = "global"
renderLocality Local = "local"

data RefNamed (m :: Mutability) (nt :: NumericType) = RefNamed Locality Text

newtype FuncNamed (is :: [NumericType]) (o :: Maybe NumericType) = FuncName Text

data SExprTarget

instance WasmTarget SExprTarget where
    type TargetFunction SExprTarget = FuncNamed
    type TargetReference SExprTarget = RefNamed

renderNumericType :: SNumericType t -> Text
renderNumericType SI32 = "i32"
renderNumericType SF32 = "f32"
renderNumericType SI64 = "i32"
renderNumericType SF64 = "f32"

renderNumericValue :: NumericVal t -> Text
renderNumericValue (NVI32 n) = T.pack (show n)
renderNumericValue (NVF32 n) = T.pack (show n)
renderNumericValue (NVI64 n) = T.pack (show n)
renderNumericValue (NVF64 n) = T.pack (show n)

instructionToSExprs :: Instruction SExprTarget is os -> [SExpr]
instructionToSExprs (InstrConcat a b) =
    instructionToSExprs a
        <> instructionToSExprs b
instructionToSExprs InstrNOP = []
instructionToSExprs InstrDrop = ["drop"]
instructionToSExprs (InstrConst ty v) =
    [ atomList
        [ renderNumericType ty <> ".const"
        , renderNumericValue v
        ]
    ]
instructionToSExprs (InstrAdd ty) = [SExprAtom (renderNumericType ty <> ".add")]
instructionToSExprs (InstrMul ty) = [SExprAtom (renderNumericType ty <> ".mul")]
instructionToSExprs (InstrSub ty) = [SExprAtom (renderNumericType ty <> ".sub")]
instructionToSExprs (InstrGlobalGet (RefNamed loc name)) =
    [ atomList
        [ renderLocality loc <> ".get"
        , name
        ]
    ]
instructionToSExprs (InstrGlobalSet (RefNamed loc name)) =
    [ atomList
        [ renderLocality loc <> ".set"
        , name
        ]
    ]
instructionToSExprs (InstrCallFunc _ (FuncName name)) = [atomList ["call", name]]
instructionToSExprs (InstrIf _ (BlockNoReturn ifTrue) (BlockNoReturn ifFalse)) =
    [ SExprAtom "if"
    , SExprList ("then" : instructionToSExprs ifTrue)
    , SExprList ("else" : instructionToSExprs ifFalse)
    ]
instructionToSExprs (InstrIf _ (BlockSingleReturn sType ifTrue) (BlockSingleReturn _ ifFalse)) =
    [ SExprList
        [ "if"
        , atomList ["result", renderNumericType sType]
        , SExprList ("then" : instructionToSExprs ifTrue)
        , SExprList ("else" : instructionToSExprs ifFalse)
        ]
    ]

functionDefinitionToSExpr :: FunctionDefinition SExprTarget is o -> SExpr
functionDefinitionToSExpr = helper [] [] 0
  where
    finalHelper paramList localList resultList body =
        SExprList
            ( ["func"]
                <> reverse paramList
                <> resultList
                <> reverse localList
                <> instructionToSExprs body
            )
    helper ::
        [SExpr] ->
        [SExpr] ->
        Int ->
        FunctionDefinition SExprTarget is' o' ->
        SExpr
    helper paramList localList n (FDWithLocal sty k) =
        helper paramList (atomList ["local", renderNumericType sty] : localList) (n + 1)
            $ k
            $ RefNamed Local
            $ T.pack
            $ show n
    helper paramList localList n (FDWithParam sty k) =
        helper (atomList ["param", renderNumericType sty] : paramList) localList (n + 1)
            $ k
            $ RefNamed Local
            $ T.pack
            $ show n
    helper paramList localList _ (FDBody (BlockSingleReturn sty is)) =
        finalHelper
            paramList
            localList
            [ SExprList
                [ "result"
                , SExprAtom $ renderNumericType sty
                ]
            ]
            is
    helper paramList localList _ (FDBody (BlockNoReturn is)) = finalHelper paramList localList [] is

moduleToSExpr :: Module SExprTarget -> SExpr
moduleToSExpr = helper (0 :: Int) (0 :: Int) []
  where
    helper _ _ soFar ModuleBase = SExprList ("module" : reverse soFar)
    helper funcCount refCount soFar (ModuleFunction fd k) =
        helper
            (funcCount + 1)
            refCount
            (functionDefinitionToSExpr fd : soFar)
            (k $ FuncName $ T.pack $ show funcCount)
    helper funcCount refCount soFar (ModuleGlobal smut (nv :: NumericVal t) k) =
        let
            valSExpr = atomList [renderNumericType (sing @t) <> ".const", renderNumericValue nv]
            tySExpr = SExprAtom $ renderNumericType (sing @t)
            refSExpr = case smut of
                SImmutable -> SExprList ["global", tySExpr, valSExpr]
                SMutable -> SExprList ["global", SExprList ["mut", tySExpr], valSExpr]
         in
            helper
                funcCount
                (refCount + 1)
                (refSExpr : soFar)
                (k $ RefNamed Global (T.pack $ show refCount))
    helper funcCount refCount soFar (ModuleExportFunc name (FuncName fn) next) =
        helper
            funcCount
            refCount
            ( SExprList ["export", SExprAtom ("\"" <> name <> "\""), atomList ["func", fn]]
                : soFar
            )
            next

moduleToWatFile :: Module SExprTarget -> FilePath -> IO ()
moduleToWatFile m path =
    T.writeFile path
        . renderStrict
        . layoutPretty defaultLayoutOptions
        . pretty
        $ moduleToSExpr m
