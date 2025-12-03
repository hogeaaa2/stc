{-# OPTIONS_GHC -Wno-partial-fields #-}

module ST.AST
  ( Program (..),
    Expr (..),
    STType (..),
    Variable (..),
    Statement (..),
    Loc (..),
    Span (..),
    Identifier,
    CaseSelector (..),
    CaseArm (..),
    Units,
    Unit (..),
    TypeDecl (..),
    LValue (..),
    ArrRange (..),
    VarInfo (..),
    VarKind (..),
    Date (..),
    TimeOfDay (..),
    DateTime (..),
    ConstVal (..),
    CallArg (..),
    GSTType (..),
    Function (..),
    FunctionBlock (..),
    CallBind (..),
    VarRetain (..),
    PartialAccess (..),
  )
where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

type POUName = Text

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
  | VKExternal -- VAR_EXTERNAL
  | VKConfig -- VAR_CONFIG
  deriving (Eq, Show)

data VarRetain
  = Retain
  | NonRetain
  deriving (Eq, Ord, Show)

data VarInfo = VarInfo
  { viType :: STType,
    viSpan :: Span,
    viKind :: VarKind,
    viConst :: Bool,
    viRetain :: Maybe VarRetain,
    viInit :: Maybe Expr
  }
  deriving (Show, Eq)

data Program = Program
  { progName :: Identifier,
    progVars :: [Variable],
    progBody :: [Statement]
  }
  deriving (Eq, Show)

data TypeDecl = TypeDecl
  { typeName :: Identifier,
    typeBody :: STType
  }
  deriving (Eq, Show)

-- ルート（翻訳単位/ファイル）

type Units = [Unit]

data Unit
  = UType [TypeDecl]
  | UProgram Program
  | UFunction Function
  | UFunctionBlock FunctionBlock
  | UGlobalVars [Variable]
  deriving (Eq, Show)

data Function = Function
  { funcName :: Identifier,
    funcRetType :: STType,
    funcVars :: [Variable],
    fBody :: [Statement]
  }
  deriving (Eq, Show)

data FunctionBlock = FunctionBlock
  { fbName :: Identifier,
    fbVars :: [Variable],
    fbBody :: [Statement]
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
  | TIME
  | LTIME
  | TOD
  | LTOD
  | DATE
  | LDATE
  | DT
  | LDT
  | FBMeta POUName
  | RefTo STType -- REF_TO T
  deriving (Eq, Show)

data Date = Date
  { dateYear :: Int,
    dateMonth :: Int,
    dateDay :: Int
  }
  deriving (Eq, Show)

data TimeOfDay = TimeOfDay
  { todHour :: Int,
    todMin :: Int,
    todSec :: Int,
    todNano :: Int -- 0..999,999,999
  }
  deriving (Eq, Show)

data DateTime = DateTime
  { dtDate :: Date,
    dtTime :: TimeOfDay
  }
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
  | ETIME Integer
  | ELTIME Integer
  | ETOD TimeOfDay
  | ELTOD TimeOfDay
  | EDATE Date
  | ELDATE Date
  | EDT DateTime
  | ELDT DateTime
  | ECall Identifier [CallArg]
  | EBit Expr PartialAccess
  | EDeref Expr -- e^
  | ERef Expr -- REF()
  deriving (Eq, Show)

data ConstVal
  = CVInt Int
  | CVBool Bool
  | CVChar Char
  | CVWChar Char
  | CVString Text
  | CVWString Text
  | -- 時刻/日付系（まずは“比較やOOBに使える形”だけを保持）
    CVTime Integer -- TIME/ LTIME（ns 総量）
  | CVTOD TimeOfDay -- TOD/LTOD（00:00:00 からの ns）
  | CVDate Date -- DATE/LDATE（year,month,day）
  | CVDT DateTime -- DT/LDT（epoch 基準のns…とりあえずns格納）
  | CVEnum Text Text -- 列挙（型名, 構成子名）
  deriving (Eq, Show)

data Variable = Variable
  { varName :: Identifier,
    varType :: STType,
    varInit :: Maybe Expr,
    varKind :: VarKind, -- VAR/INPUT/OUTPUT/...
    varConst :: Bool, -- CONSTANT
    varRetain :: Maybe VarRetain -- RETAIN / NON_RETAIN / なし
  }
  deriving (Eq, Show)

data LValue
  = LVar Identifier
  | LField LValue Identifier
  | LIndex LValue [Expr]
  deriving (Eq, Show)

data CallBind
  = CallIn Identifier Expr
  | CallOut Identifier LValue
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
  | FBCall Identifier [CallBind]
  | Skip -- 空文
  deriving (Eq, Show)

-- CASE ラベルは「定数式」を許すため Expr ベースに
data CaseSelector
  = CSExpr Expr -- 例: 1+2, Color.Red
  | CSRangeE Expr Expr -- 例: 1..(2+3)
  deriving (Eq, Show)

data CaseArm = CaseArm [CaseSelector] [Statement] -- selectors : stmts ;
  deriving (Eq, Show)

-- 関数呼び出しの引数:
--   Inc(1, 2)
--   Inc(x := 1, y := 2)
data CallArg
  = CallArgPos Expr -- 位置引数
  | CallArgNamed Identifier Expr -- 名前付き引数
  deriving (Eq, Show)

-- ANY_* クラス
data GSTType
  = GSTAny
  | GSTAnyInt
  | GSTAnyNum
  | GSTAnyReal
  | GSTAnyBit
  | GSTAnyString
  | GSTAnyDate
  | GSTAnyDuration
  deriving (Eq, Show)

data PartialAccess
  = PAIndex Int -- .7 や .%X7
  | PAByte Int -- .%B3  (BYTE単位)
  | PAWord Int -- .%W1  (WORD単位)
  | PADword Int -- .%D0  (DWORD単位)
  deriving (Eq, Show)
