module TypedWasm.Util.SExpr where

import Data.Text (Text)
import GHC.Exts (IsString, fromString)
import Prettyprinter

data SExpr
    = SExprAtom Text
    | SExprList [SExpr]
    deriving stock (Eq, Show)

instance IsString SExpr where
    fromString = SExprAtom . fromString

instance Pretty SExpr where
    pretty (SExprAtom t) = pretty t
    pretty (SExprList cs) = parens $ group $ nest 1 $ vsep $ map pretty cs

atomList :: [Text] -> SExpr
atomList = SExprList . map SExprAtom
