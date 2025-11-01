{-# OPTIONS_GHC -Wno-partial-fields #-}

module ST.AST
  ( Program (..),
    VarDecls (..),
    Expr (..),
    STType (..),
    Variable (..),
    Statement (..),
    Loc (..),
    Span (..),
    Identifier,
    CaseSelector (..),
    CaseArm (..),
    Unit (..),
    TypeDecl (..),
    LValue (..),
    ArrRange (..),
    VarInfo (..),
    VarKind (..),
  )
where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data Span = Span
  { spanStart :: SourcePos,
    spanEnd :: SourcePos
  }
  deriving (Eq, Show)

data Loc a = Loc
  { locSpan :: Span,
    locVal :: a
  }
  deriving (Eq, Show)

type Identifier = Loc Text

data VarKind
  = VKLocal -- VAR
  | VKInput -- VAR_INPUT
  | VKOutput -- VAR_OUTPUT
  | VKInOut -- VAR_INOUT
  | VKTemp -- VAR_TEMP
  | VKGlobal -- VAR_GLOBAL
  | VKConstant -- VAR CONSTANT
  deriving (Eq, Show)

data VarInfo = VarInfo
  { viType :: STType,
    viSpan :: Span,
    viKind :: VarKind,
    viInit :: Maybe Expr
  }
  deriving (Show, Eq)

data Program = Program
  { progName :: Text,
    varDecls :: VarDecls,
    progBody :: [Statement]
  }
  deriving (Eq, Show)

-- var_declarations
newtype VarDecls = VarDecls [Variable] deriving (Eq, Show)

data TypeDecl = TypeDecl
  { typeName :: Identifier,
    typeBody :: STType
  }
  deriving (Eq, Show)

-- ルート（翻訳単位/ファイル）
data Unit = Unit
  { uTypes :: [TypeDecl],
    uPrograms :: [Program]
  }
  deriving (Eq, Show)

data ArrRange = ArrRange Int Int
  deriving (Eq, Show)

data STType
  = BOOL
  | SINT
  | INT
  | DINT
  | LINT
  | USINT
  | UINT
  | UDINT
  | ULINT
  | REAL
  | LREAL
  | STRING (Maybe Int)
  | WSTRING (Maybe Int)
  | CHAR
  | WCHAR
  | BYTE
  | WORD
  | DWORD
  | LWORD
  | Named Identifier
  | Struct [(Identifier, STType)]
  | Array [ArrRange] STType
  | Enum [(Identifier, Maybe Expr)]
  deriving (Eq, Show)

data Expr
  = EINT Int
  | EBOOL Bool
  | EREAL Double
  | ELREAL Double
  | ESTRING Text
  | EWSTRING Text
  | ECHAR Char
  | EWCHAR Char
  | ENeg Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | EEq Expr Expr
  | ENe Expr Expr
  | ELt Expr Expr
  | ELe Expr Expr
  | EGt Expr Expr
  | EGe Expr Expr
  | ENot Expr
  | EAnd Expr Expr
  | EXor Expr Expr
  | EOr Expr Expr
  | EVar Identifier
  | EField Expr Identifier
  | EIndex Expr [Expr]
  | EEnum Identifier Identifier
  | EArrayAgg [Expr]
  | EStructAgg [(Identifier, Expr)]
  deriving (Eq, Show)

data Variable = Variable
  { varName :: Identifier,
    varType :: STType,
    varInit :: Maybe Expr,
    varConst :: Bool
  }
  deriving (Eq, Show)

data LValue
  = LVar Identifier
  | LField LValue Identifier
  | LIndex LValue [Expr]
  deriving (Eq, Show)

data Statement
  = Assign LValue Expr
  | If
      { ifCond :: Expr,
        ifThen :: [Statement],
        ifElsifs :: [(Expr, [Statement])],
        ifElse :: [Statement]
      }
  | While Expr [Statement] -- WHILE <cond> DO <stmts> END_WHILE
  | Repeat [Statement] Expr -- REPEAT <stmts> UNTIL <cond> END_REPEAT
  | Case Expr [CaseArm] [Statement] -- CASE <e> OF <arms> [ELSE <stmts>] END_CASE
  | For Identifier Expr Expr (Maybe Expr) [Statement] -- FOR i := init TO end [BY step] DO body END_FOR
  | Skip -- 空文
  deriving (Eq, Show)

-- CASE ラベルは「定数式」を許すため Expr ベースに
data CaseSelector
  = CSExpr Expr -- 例: 1+2, Color.Red
  | CSRangeE Expr Expr -- 例: 1..(2+3)
  deriving (Eq, Show)

data CaseArm = CaseArm [CaseSelector] [Statement] -- selectors : stmts ;
  deriving (Eq, Show)
