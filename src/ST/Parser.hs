{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module ST.Parser
  ( parseUnits,
    parseProgram,
    parseUnit',
    parseUnits',
    identifier,
    pUnit,
    pStmt,
    pExpr,
    pTypeDecl,
    pVariable,
    pInt,
    pReal,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
  ( Operator (..),
    makeExprParser,
  )
import Data.Bifunctor (first)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, toUpper)
import Data.Functor (($>))
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Scientific (Scientific)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import ST.AST
import Text.Megaparsec
  ( MonadParsec (..),
    Parsec,
    anySingle,
    atEnd,
    between,
    choice,
    empty,
    eof,
    getOffset,
    getSourcePos,
    lookAhead,
    many,
    manyTill,
    observing,
    oneOf,
    optional,
    parse,
    registerParseError,
    satisfy,
    sepBy,
    sepBy1,
    some,
    someTill,
    withRecovery,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    space1,
    string,
    string',
  )
import qualified Text.Megaparsec.Char.Lexer as L
  ( decimal,
    lexeme,
    skipBlockCommentNested,
    skipLineComment,
    space,
    symbol',
  )
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (initialPos)

type Parser = Parsec Void Text

data DollarPolicy = DollarPolicy
  { allowDollarSingle :: Bool,
    allowDollarDouble :: Bool
  }

----------------------------------
-- public functions
----------------------------------

parseUnits ::
  [(FilePath, Text)] ->
  Either (FilePath, ParseErrorBundle Text Void) [Unit]
parseUnits = traverse (\(fp, src) -> first (\e -> (fp, e)) (parseUnit fp src))

-- 複数ファイル（擬似パスを自動付与）→ [Unit]
parseUnits' :: [Text] -> Either (Int, ParseErrorBundle Text Void) [Unit]
parseUnits' srcs =
  traverse one (zip [1 ..] srcs)
  where
    one (i, s) = first' ((,) i) $ parseUnit ("<input-" <> show i <> ">") s
    first' f = either (Left . f) Right

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse (pProgram <* eof) "<input>"

parseUnit :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Unit
parseUnit = parse (sc *> pUnit <* eof)

parseUnit' :: Text -> Either (ParseErrorBundle Text Void) Unit
parseUnit' = parse (sc *> pUnit <* eof) "<input>"

-- pUnit :: Parser Unit
-- pUnit = do
--   items <- many pUnitItem
--   pure $ Unit items

pUnit :: Parser Unit
pUnit =
  choice
    [ UType <$> try pTypeDecl,
      UProgram <$> try pProgram,
      UFunctionBlock <$> try pFunctionBlock,
      UFunction <$> pFunction
    ]

pTypeDecl :: Parser [TypeDecl]
pTypeDecl = lexeme $ do
  _ <- symbol "TYPE"
  someTill pOneType (symbol "END_TYPE")
  where
    pOneType = do
      nm <- identifier
      _ <- symbol ":"
      ty <- pSTType
      _ <- symbol ";"
      pure (TypeDecl nm ty)

pProgram :: Parser Program
pProgram = do
  _ <- symbol "PROGRAM"
  pn <- identifier
  vds <- some pVarDecls
  let vars = concat [vs | VarDecls vs <- vds]
  body <- many pStmt
  pure (Program pn (VarDecls vars) body)

pVarDecls :: Parser VarDecls
pVarDecls = lexeme $ do
  kind <-
    choice
      [ VKInOut <$ try (symbol "VAR_IN_OUT"),
        VKInput <$ try (symbol "VAR_INPUT"),
        VKOutput <$ try (symbol "VAR_OUTPUT"),
        VKLocal <$ symbol "VAR"
      ]
  isConst <- isJust <$> optional (symbol "CONSTANT")
  vs <- manyTill (pVariable kind isConst) (symbol "END_VAR")
  pure (VarDecls vs)

pFunction :: Parser Function
pFunction = do
  _ <- symbol "FUNCTION"
  fname <- identifier
  _ <- symbol ":"
  ftype <- pSTType
  vds <- some pVarDecls
  let vars = concat [vs | VarDecls vs <- vds]
  body <- many pStmt
  pure (Function fname ftype (VarDecls vars) body)

pFunctionBlock :: Parser FunctionBlock
pFunctionBlock = lexeme $ do
  _ <- symbol "FUNCTION_BLOCK"
  fbname <- identifier
  vds <- some pVarDecls
  let vars = concat [vs | VarDecls vs <- vds]
  body <- many pStmt
  pure (FunctionBlock fbname (VarDecls vars) body)

-- FB 呼び出しの 1 つの束縛
pCallBind :: Parser CallBind
pCallBind = lexeme $ do
  nm <- identifier
  -- "=>" 優先（出力バインド）→ 次に ":="（入力/IN_OUT）
  choice
    [ CallOut nm <$> (symbol "=>" *> (pBaseVarL >>= pPostfixL)),
      CallIn nm <$> (symbol ":=" *> pExpr)
    ]

-- FB 呼び出し「文」:  f( ... );
pFBCallStmt :: Parser Statement
pFBCallStmt = lexeme $ do
  f <- identifier
  _ <- symbol "("
  binds <- pCallBind `sepBy` symbol ","
  _ <- symbol ")"
  _ <- semicolon
  pure (FBCall f binds)

pStmt :: Parser Statement
pStmt = withRecovery recover pStmtCore
  where
    pStmtCore =
      choice
        [ pIf,
          pWhile,
          pRepeat,
          pCase,
          pFor,
          try $ lookAhead (identifier *> symbol "(") *> pFBCallStmt,
          try pAssign,
          pSkip
        ]

    recover err = do
      registerParseError err
      done <- atEnd
      if done
        then empty
        else do
          -- すでに同期トークン直前なら、ここでも止める（親の manyTill が拾う）
          let syncHere =
                void . lookAhead $
                  choice $
                    map
                      symbol
                      [ ";",
                        "ELSIF",
                        "ELSE",
                        "END_IF",
                        "END_WHILE",
                        "UNTIL",
                        "END_REPEAT",
                        "END_CASE"
                      ]
          m <- observing syncHere
          case m of
            Right _ -> empty -- ここで止めて親に任せる
            Left _ -> do
              -- それ以外は同期点まで“進めてから”ダミーを返す
              skipToStmtSync
              pure (If (EBOOL True) [] [] [])

pExpr :: Parser Expr
pExpr = makeExprParser pTerm table
  where
    pTerm = pPrimaryE >>= pPostfixE
    table =
      [ -- precedence 9
        [ prefix "-" ENeg,
          prefix "+" id,
          prefix "NOT" ENot
        ],
        -- Exponentiation (precedence 8) is defined in IEC61131-3
        -- but not supported in CODESYS.
        -- https://content.helpme-codesys.com/en/CODESYS%20Development%20System/_cds_st_expressions.html

        -- precedence 7
        [ binary "*" EMul,
          binary "/" EDiv,
          binary "MOD" EMod
        ],
        -- precedence 6
        [ binary "+" EAdd,
          binary "-" ESub
        ],
        -- precedence 5&4
        [ comp "<=" ELe,
          comp ">=" EGe,
          comp "<>" ENe,
          comp "<" ELt,
          comp ">" EGt,
          comp "=" EEq
        ],
        -- precedence 3
        [binary "AND" EAnd],
        -- precedence 2
        [binary "XOR" EXor],
        -- precedence 1
        [binary "OR" EOr]
      ]

    binary, comp :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)
    comp name f = InfixN (f <$ symbol name)
    -- exp name f = InfixR (f <$ symbol name)

    prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
    prefix name f = Prefix (f <$ symbol name)

-- postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
-- postfix name f = Postfix (f <$ symbol name)

-- pLValue :: Parser LValue
-- pLValue = do
--   x <- identifier
--   fs <- many (symbol "." *> identifier)
--   pure (foldl LField (LVar x) fs)

-- Var_Decl_Init
pVariable :: VarKind -> Bool -> Parser Variable
pVariable kind isConst = lexeme $ do
  name <- identifier
  _ <- colon
  dt <- pSTType
  vInit <- optional (assignOp *> lexeme pExpr)
  _ <- semicolon
  pure
    Variable
      { varName = name,
        varType = dt,
        varInit = vInit,
        varConst = isConst,
        varKind = kind,
        varRetain = False
      }

pInt :: Parser Int
pInt = lexeme $ do
  -- 先頭符号（オプション）
  msign <- optional (char '+' <|> char '-')
  mbBase <-
    optional . try $
      choice
        [ 2 <$ string "2#",
          8 <$ string "8#",
          10 <$ string "10#",
          16 <$ string "16#"
        ]
  n <- case mbBase of
    -- 基数付き：CODESYS 準拠で先頭/末尾 '_' も許容。ただし "__" は不可。
    Just b -> do
      let isDigitB c
            | b == 2 = c == '0' || c == '1'
            | b == 8 = isOctDigit c
            | b == 10 = isDigit c
            | b == 16 = isHexDigit c
            | otherwise = False
      raw <- some (satisfy (\c -> isDigitB c || c == '_'))
      -- 少なくとも1桁の“基数桁”を含む & "__" が無い
      if any isDigitB raw && noDoubleUnders raw
        then pure (readBaseInt b raw)
        else fail "invalid based integer literal"
    -- 10進：桁間 '_' のみ許容（先頭/末尾/連続不可）
    Nothing -> do
      read . stripUnders <$> decDigitsStrict
  -- 符号を適用
  pure $ case msign of
    Just '-' -> negate n
    _ -> n

-- 10進 REAL リテラル（最小）： 1.0 / 0.5 / 12e-3 / 3. / 0E+5 など
-- 仕様: 先頭に少なくとも1桁、(小数 or 指数) のどちらか必須
pReal :: Parser Double
pReal = lexeme $ do
  -- 整数部：末尾 '_' も許容（直後に '.' が来る前提）
  intPart <- decDigitsAllowTrailUnders
  _ <- char '.'
  -- 小数部：先頭 '_' も許容（直前が '.'）
  fracPart <- decDigitsAllowLeadUnders

  -- 指数部（任意）：桁間 '_' は許容、先頭/末尾 '_' は不可
  mExp <- optional $ do
    e <- oneOf ['e', 'E']
    s <- optional (oneOf ['+', '-'])
    d0 <- digitChar
    ds <- many (digitChar <|> try (char '_' *> digitChar))
    let digits = d0 : ds
    pure (e : maybeToList s ++ stripUnders digits)

  let numStr = stripUnders intPart ++ "." ++ stripUnders fracPart ++ fromMaybe "" mExp
  pure (read numStr)

----------------------------------
-- private functions
----------------------------------

stringPolicy, wstringPolicy :: DollarPolicy
stringPolicy = DollarPolicy {allowDollarSingle = True, allowDollarDouble = False} -- STRING
wstringPolicy = DollarPolicy {allowDollarSingle = False, allowDollarDouble = True} -- WSTRING

reserved :: Set Text
reserved =
  Set.fromList
    [ "PROGRAM",
      "VAR",
      "END_VAR",
      "FUNCTION",
      "END_FUNCTION",
      "FUNCTION_BLOCK",
      "END_FUNCTION_BLOCK",
      "VAR_INPUT",
      "VAR_OUTPUT",
      "VAR_IN_OUT",
      "VAR_TEMP",
      "VAR_GLOBAL",
      "INT",
      "BOOL",
      "TRUE",
      "FALSE",
      "CONSTANT",
      "NOT",
      "AND",
      "OR",
      "IF",
      "THEN",
      "ELSIF",
      "ELSE",
      "END_IF",
      "WHILE",
      "DO",
      "END_WHILE",
      "REPEAT",
      "UNTIL",
      "END_REPEAT",
      "CASE",
      "OF",
      "END_CASE",
      "TYPE",
      "END_TYPE",
      "STRUCT",
      "END_STRUCT",
      "ARRAY",
      "FOR",
      "TO",
      "BY",
      "END_FOR",
      "MOD",
      "XOR",
      "REAL",
      "LREAL",
      "STRING",
      "SINT",
      "DINT",
      "LINT",
      "USINT",
      "UINT",
      "UDINT",
      "ULINT",
      "BYTE",
      "WORD",
      "DWORD",
      "LWORD"
    ]

withSpan :: Parser a -> Parser (Loc a)
withSpan p = do
  s <- getSourcePos
  x <- p
  e <- getSourcePos
  pure (Loc (Span s e) x)

-- 失敗を飲み込み、エラーだけ登録して続行（Nothing で返す）
soft :: Parser a -> Parser (Maybe a)
soft p = do
  r <- observing (try p)
  case r of
    Right x -> pure (Just x)
    Left e -> registerParseError e >> pure Nothing

-- 必須キーワード（無ければキーワードをエラー報告して続行）
kw :: Text -> Parser ()
kw t = void $ softLabel t $ symbol t

-- 文レベルの同期点まで読み飛ばす（; / ELSIF / ELSE / END_* / EOF）
skipToStmtSync :: Parser ()
skipToStmtSync =
  void $ manyTill anySingle ender
  where
    syncTok :: Parser Text
    syncTok =
      choice
        ( map
            symbol
            [ ";",
              "ELSIF",
              "ELSE",
              "END_IF",
              "END_WHILE",
              "UNTIL",
              "END_REPEAT",
              "END_CASE"
            ]
        )
    ender :: Parser ()
    ender = (void . lookAhead $ syncTok) <|> eof

-- 「期待 <label>」を現在位置に1件登録（失敗はしない）
registerExpectLabel :: Text -> Parser ()
registerExpectLabel lbl = do
  off <- getOffset
  let item = Label (NE.fromList (T.unpack lbl)) -- expecting <label>
  registerParseError (TrivialError off Nothing (Set.singleton item))

-- 失敗したら「期待 <label>」を登録して続行
softLabel :: Text -> Parser a -> Parser (Maybe a)
softLabel lbl p = do
  r <- observing (try p)
  case r of
    Right x -> pure (Just x)
    Left _ -> registerExpectLabel lbl >> pure Nothing

unknownSpan :: Span
unknownSpan = let p = initialPos "<unknown>" in Span p p

-- readBaseInt :: Int -> String -> Maybe Int
-- readBaseInt b s =
--   case readInt b (isDigitBase b) valBase s of
--     [(n, "")] -> Just n
--     _ -> Nothing

stripUnders :: String -> String
stripUnders = filter (/= '_')

noDoubleUnders :: String -> Bool
noDoubleUnders s = not ("__" `isInfixOf` s)

-- 10進「桁間のみ '_'」許容（先頭・末尾・連続不可）
decDigitsStrict :: Parser String
decDigitsStrict = do
  d0 <- digitChar
  ds <- many (digitChar <|> try (char '_' *> digitChar))
  pure (d0 : ds)

-- 10進「末尾に '_' が来ても OK」（直後に '.' が来る想定の整数部用）
decDigitsAllowTrailUnders :: Parser String
decDigitsAllowTrailUnders = do
  d0 <- digitChar
  ds <- many (digitChar <|> char '_') -- ここでは末尾 '_' を許容
  pure (d0 : ds)

-- 10進「先頭に '_' があっても OK」（直前が '.' の小数部先頭用）
decDigitsAllowLeadUnders :: Parser String
decDigitsAllowLeadUnders = do
  us <- many (char '_') -- 前置 '_' を許容
  d0 <- digitChar -- 必ずどこかで1桁は必要
  ds <- many (digitChar <|> try (char '_' *> digitChar))
  pure (replicate (length us) '_' ++ (d0 : ds))

readBaseInt :: Int -> String -> Int
readBaseInt b s0 =
  let s = stripUnders s0
      val c
        | b == 16 =
            let x = toUpper c
             in if isDigit c
                  then fromEnum c - 48
                  else fromEnum x - 55 -- 'A'.. -> 10..
        | otherwise = fromEnum c - 48
   in foldl (\acc c -> acc * b + val c) 0 s

delimOf :: DollarPolicy -> Char
delimOf pol = if allowDollarSingle pol then '\'' else '"'

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- キーワード（エラー報告なし）
symbol :: Text -> Parser Text
symbol = L.symbol' sc

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c

isLetterDigit :: Char -> Bool
isLetterDigit c = isLetter c || isDigit c

semicolon :: Parser Char
semicolon = lexeme $ char ';'

colon :: Parser Char
colon = lexeme $ char ':'

assignOp :: Parser Text
assignOp = symbol ":="

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

identifier :: Parser Identifier
identifier = withSpan $ lexeme $ do
  firstTxt <- choice [l, usld]
  rest <- many $ choice [ld, usld]
  let result = T.append firstTxt (T.concat rest)
   in if not $ Set.member (T.toUpper result) reserved
        then pure result
        else do
          off <- getOffset
          let item = Label (NE.fromList (T.unpack result))
          registerParseError
            ( TrivialError
                off
                (Just item)
                (Set.singleton $ Label $ NE.fromList "non-reserved string")
            )
          pure ""
  where
    l :: Parser Text
    l = T.singleton <$> satisfy isLetter

    ld :: Parser Text
    ld = T.singleton <$> satisfy isLetterDigit

    usld :: Parser Text
    usld = do
      us <- char '_'
      c <- satisfy isLetterDigit
      pure $ T.cons us (T.singleton c)

pBool :: Parser Bool
pBool = (True <$ symbol "TRUE") <|> (False <$ symbol "FALSE")

pTypedReal :: Parser Expr
pTypedReal =
  choice
    [ ELREAL <$> (symbol "LREAL" *> symbol "#" *> pReal),
      EREAL <$> (symbol "REAL" *> symbol "#" *> pReal)
    ]

-- 文字列リテラル：ポリシーを受け取る版
pStringLitWith :: DollarPolicy -> (Text -> Expr) -> Parser Expr
pStringLitWith pol strType = lexeme $ do
  delim <- char $ delimOf pol
  txt <- T.pack <$> manyTill (strCharWith pol delim) (char delim)
  pure $ strType txt

pCharLitWith :: DollarPolicy -> (Char -> Expr) -> Parser Expr
pCharLitWith pol chrType = lexeme $ do
  delim <- char $ delimOf pol
  c <- strCharWith pol delim
  _ <- char $ delimOf pol
  pure $ chrType c

pStringLit :: Parser Expr
pStringLit = pStringLitWith stringPolicy ESTRING

pWStringLit :: Parser Expr
pWStringLit = pStringLitWith wstringPolicy EWSTRING

pCharLit :: Parser Expr
pCharLit = pCharLitWith stringPolicy ECHAR

pWCharLit :: Parser Expr
pWCharLit = pCharLitWith wstringPolicy EWCHAR

-- 1 文字（通常 or $エスケープ）
strCharWith :: DollarPolicy -> Char -> Parser Char
strCharWith pol delim =
  choice
    [ char '$' *> dollarEscape pol,
      satisfy (\c -> c /= delim && c /= '\n' && c /= '\r')
    ]

dollarEscape :: DollarPolicy -> Parser Char
dollarEscape pol = label "escape" $ do
  c <- satisfy (\x -> isAsciiLower x || isAsciiUpper x || x == '$' || x == '\'' || x == '"')
  case c of
    '$' -> pure '$'
    '\'' ->
      if allowDollarSingle pol
        then pure '\''
        else fail "$' escape is not allowed for this string type"
    '"' ->
      if allowDollarDouble pol
        then pure '"'
        else fail "$\" escape is not allowed for this string type"
    'L' -> pure '\n'
    'l' -> pure '\n'
    -- \$L/$l → LF
    'N' -> pure '\n'
    'n' -> pure '\n'
    -- \$N/$n → LF（CODESYS に合わせて $L と同義）
    'P' -> pure '\f'
    'p' -> pure '\f'
    -- \$P/$p → FF
    'R' -> pure '\r'
    'r' -> pure '\r'
    -- \$R/$r → CR
    'T' -> pure '\t'
    't' -> pure '\t'
    -- \$T/$t → TAB
    _ -> fail "unknown $-escape"

pPrimaryE :: Parser Expr
pPrimaryE =
  choice
    [ pArrayAgg,
      pStructAgg,
      parens pExpr,
      try pCharLit,
      try pWCharLit,
      try pLDTLit,
      try pDTLit,
      try pLTODLit,
      try pTODLit,
      try pLDateLit,
      try pDateLit,
      try pLTimeLit,
      try pTimeLit,
      pStringLit,
      pWStringLit,
      EBOOL <$> try pBool,
      try pTypedReal,
      EREAL <$> try pReal, -- 無印はREAL扱い
      EINT <$> try pInt,
      try (ECall <$> identifier <*> pCallArgs),
      EVar <$> identifier
    ]

dot1 :: Parser ()
dot1 = void $ lexeme $ try (char '.' *> notFollowedBy (char '.'))

-- フィールド/添字のポストフィックス（式用）
pPostfixE :: Expr -> Parser Expr
pPostfixE = go
  where
    go e = do
      m <- optional (choice [field e, index e])
      maybe (pure e) go m

    field e' = do
      dot1
      EField e' <$> identifier

    index e'' = do
      is <- brackets (pExpr `sepBy1` symbol ",")
      pure (EIndex e'' is)

-- フィールド/添字のポストフィックス（左辺用）
pPostfixL :: LValue -> Parser LValue
pPostfixL = go
  where
    go l = do
      m <- optional (choice [field l, index l])
      maybe (pure l) go m

    field l' = do
      dot1
      LField l' <$> identifier

    index l'' = do
      is <- brackets (pExpr `sepBy1` symbol ",")
      pure (LIndex l'' is)

-- 構造体集成: (name := expr, ...)
pStructAgg :: Parser Expr
pStructAgg = lexeme . try $ do
  _ <- symbol "("
  -- 先読みで "識別子 :=" を確認
  i0 <- identifier
  _ <- symbol ":="
  e0 <- pExpr
  rest <- many (symbol "," *> ((,) <$> identifier <* symbol ":=" <*> pExpr))
  _ <- symbol ")"
  pure $ EStructAgg ((i0, e0) : rest)

-- 配列集成: [expr, expr, ...]
pArrayAgg :: Parser Expr
pArrayAgg = lexeme $ do
  _ <- symbol "["
  es <- pExpr `sepBy` symbol ","
  _ <- symbol "]"
  pure $ EArrayAgg es

pSTType :: Parser STType
pSTType =
  choice
    [ pStructType,
      pEnumType,
      pArrayType,
      SINT <$ symbol "SINT",
      INT <$ symbol "INT",
      DINT <$ symbol "DINT",
      LINT <$ symbol "LINT",
      USINT <$ symbol "USINT",
      UINT <$ symbol "UINT",
      UDINT <$ symbol "UDINT",
      ULINT <$ symbol "ULINT",
      BYTE <$ symbol "BYTE",
      WORD <$ symbol "WORD",
      DWORD <$ symbol "DWORD",
      LWORD <$ symbol "LWORD",
      BOOL <$ symbol "BOOL",
      REAL <$ symbol "REAL",
      LREAL <$ symbol "LREAL",
      CHAR <$ symbol "CHAR",
      WCHAR <$ symbol "WCHAR",
      LDT <$ (try (string' "LDATE_AND_TIME") <|> string' "LDT"),
      LTOD <$ (try (string' "LTIME_OF_DAY") <|> string' "LTOD"),
      LTIME <$ string' "LTIME",
      LDATE <$ string' "LDATE",
      DT <$ (try (string' "DATE_AND_TIME") <|> string' "DT"),
      TOD <$ (try (string' "TIME_OF_DAY") <|> string' "TOD"),
      TIME <$ string' "TIME",
      DATE <$ string' "DATE",
      pStringType,
      pWStringType,
      Named <$> identifier
    ]

pWStringType :: Parser STType
pWStringType = do
  _ <- symbol "WSTRING"
  mlen <- optional (parens L.decimal <|> brackets L.decimal)
  pure (WSTRING mlen)

-- STRING または STRING(n) / STRING[n] を受理し、長さを保持
pStringType :: Parser STType
pStringType = do
  _ <- symbol "STRING"
  mlen <- optional (parens L.decimal <|> brackets L.decimal)
  pure (STRING mlen)

-- ( Red := 0, Green, Blue := 1+2 )
pEnumType :: Parser STType
pEnumType = do
  _ <- symbol "("
  xs <- pOne `sepBy1` symbol ","
  _ <- symbol ")"
  pure (Enum xs)
  where
    pOne = do
      nm <- identifier
      mv <- optional (symbol ":=" *> pExpr)
      pure (nm, mv)

pArrayType :: Parser STType
pArrayType = do
  _ <- symbol "ARRAY"
  _ <- symbol "["
  rs <- pIdxRange `sepBy1` symbol ","
  _ <- symbol "]"
  kw "OF"
  Array rs <$> pSTType
  where
    pIdxRange = do
      lo <- pInt
      _ <- symbol ".."
      ArrRange lo <$> pInt

pStructType :: Parser STType
pStructType = do
  _ <- symbol "STRUCT"
  fields <- manyTill pField (symbol "END_STRUCT")
  pure (Struct fields)
  where
    pField = do
      f <- identifier
      _ <- symbol ":"
      t <- pSTType
      _ <- symbol ";"
      pure (f, t)

pSkip :: Parser Statement
pSkip = semicolon $> Skip

pAssign :: Parser Statement
pAssign = do
  lhs <- pBaseVarL >>= pPostfixL
  -- ":=" を必須扱いで
  r1 <- observing assignOp
  case r1 of
    Left e -> registerParseError e
    _ -> pure ()
  rhs <- fromMaybe (EINT 0) <$> soft pExpr -- rhs 欠落でも前進
  -- ';' も必須扱い
  r2 <- observing (symbol ";")
  case r2 of
    Left e -> registerParseError e
    _ -> pure ()
  pure (Assign lhs rhs)

pIf :: Parser Statement
pIf = do
  _ <- symbol "IF"
  mc <- softLabel "Condition" pExpr
  let c0 = fromMaybe (EBOOL True) mc
  kw "THEN"
  th0 <- manyTill pStmt ((void . lookAhead $ ender) <|> eof)
  elsifs <- many $ do
    _ <- symbol "ELSIF"
    mcE <- softLabel "Condition" pExpr
    let ce = fromMaybe (EBOOL True) mcE
    kw "THEN"
    thE <- manyTill pStmt ((void . lookAhead $ ender) <|> eof)
    pure (ce, thE)

  mElse <- optional (symbol "ELSE" *> manyTill pStmt (symbol "END_IF"))
  case mElse of
    Just els -> pure (If c0 th0 elsifs els)
    Nothing -> do
      -- END_IF は “無いときだけ”登録。あれば消費して終わり。
      r <- observing (symbol "END_IF")
      case r of
        Right _ -> pure (If c0 th0 elsifs [])
        Left _ -> registerExpectLabel "END_IF" >> pure (If c0 th0 elsifs [])
  where
    -- ← ここは必ず symbol（kw を使うと見るだけで診断が乗る）
    ender = symbol "ELSIF" <|> symbol "ELSE" <|> symbol "END_IF"

-- WHILE <expr> DO <stmts> END_WHILE
pWhile :: Parser Statement
pWhile = do
  _ <- symbol "WHILE"
  mc <- softLabel "Condition" pExpr
  let cond = fromMaybe (EBOOL True) mc
  kw "DO"
  body <- manyTill pStmt ((void . lookAhead $ symbol "END_WHILE") <|> eof)
  r <- observing (symbol "END_WHILE")
  case r of
    Right _ -> pure (While cond body)
    Left _ -> registerExpectLabel "END_WHILE" >> pure (While cond body)

-- REPEAT <stmts> UNTIL <expr> END_REPEAT
pRepeat :: Parser Statement
pRepeat = do
  _ <- symbol "REPEAT"
  body <- manyTill pStmt ((void . lookAhead $ symbol "UNTIL") <|> eof)
  -- UNTIL が無ければ "UNTIL" だけ
  rU <- observing (symbol "UNTIL")
  case rU of
    Right _ -> pure ()
    Left _ -> registerExpectLabel "UNTIL"
  mc <- softLabel "Condition" pExpr
  let cond = fromMaybe (EBOOL True) mc
  rE <- observing (symbol "END_REPEAT")
  case rE of
    Right _ -> pure (Repeat body cond)
    Left _ -> registerExpectLabel "END_REPEAT" >> pure (Repeat body cond)

-- CASE <expr> OF <arm>* [ELSE <stmts>] END_CASE
pCase :: Parser Statement
pCase = do
  _ <- symbol "CASE"
  me <- softLabel "Expression" pExpr
  kw "OF"
  let scrut = fromMaybe (EINT 0) me
  arms <- manyTill pCaseArm (lookAhead (symbol "ELSE" <|> symbol "END_CASE"))
  mEls <- optional (symbol "ELSE" *> manyTill pStmt (symbol "END_CASE"))
  case mEls of
    Just els -> pure (Case scrut arms els)
    Nothing -> do
      r <- observing (symbol "END_CASE")
      case r of
        Right _ -> pure (Case scrut arms [])
        Left e -> registerParseError e >> pure (Case scrut arms [])

-- <selectors> ':' <stmts> ';'
-- selectors: sel (',' sel)*
pCaseArm :: Parser CaseArm
pCaseArm = do
  sels <- pSelector `sepBy1` symbol ","
  _ <- symbol ":"
  body <- manyTill pStmt endArm
  pure (CaseArm sels body)
  where
    endArm :: Parser ()
    endArm =
      lookAhead $
        choice
          [ void (symbol "ELSE"),
            void (symbol "END_CASE"),
            void (try (pSelector `sepBy1` symbol "," *> symbol ":" <* notFollowedBy (char '=')))
          ]

pSelector :: Parser CaseSelector
pSelector = do
  a <- pExpr
  m <- optional (symbol ".." *> pExpr)
  case m of
    Nothing -> pure (CSExpr a)
    Just b -> pure (CSRangeE a b)

-- -- 便利：識別子起点の“素”デザインタ
-- pBaseVarE :: Parser Expr
-- pBaseVarE = EVar <$> identifier

pBaseVarL :: Parser LValue
pBaseVarL = LVar <$> identifier

-- pEnumLit :: Parser Expr
-- pEnumLit = do
--   ty <- identifier
--   _ <- symbol "."
--   EEnum ty <$> identifier

-- FOR i := init TO end [BY step] DO stmts END_FOR
pFor :: Parser Statement
pFor = do
  _ <- symbol "FOR"

  -- ① iv := init を “ひとかたまり” で読む（無ければ 1 回だけ Expression 診断）
  mInit <- optional . try $ do
    iv' <- identifier
    _ <- assignOp
    e0' <- pExpr
    pure (iv', e0')

  (iv, e0) <- case mInit of
    Just pair -> pure pair
    Nothing -> do
      _ <- softLabel "Control Initialization" pExpr
      pure (Loc unknownSpan "<for-var>", EINT 0)

  kw "TO" -- 必須

  -- ② end 式：無ければ Expression を 1 回登録し、0 で続行
  mE1 <- softLabel "Expression" pExpr
  let e1 = fromMaybe (EINT 0) mE1

  -- ③ BY があれば step 式：無ければ Expression を登録し、1 にフォールバック
  mby <- optional $ do
    _ <- symbol "BY"
    me <- softLabel "Expression" pExpr
    pure (fromMaybe (EINT 1) me)

  kw "DO" -- 必須

  -- ④ 本体：END_FOR 欠落時は診断を積んで空ブロックで回復
  body <-
    withRecovery
      ( \_ -> do
          _ <- softLabel "END_FOR" (symbol "END_FOR")
          pure []
      )
      (manyTill pStmt (symbol "END_FOR"))

  pure (For iv e0 e1 mby body)

-- 小数（例: "14" or "14.7"）。Scientific で返す
pNumberSci :: Parser Scientific
pNumberSci = lexeme $ do
  intPart <- some digitChar
  mFrac <- optional (char '.' *> some digitChar)
  let txt = case mFrac of
        Nothing -> intPart
        Just f -> intPart <> "." <> f
  case reads txt of
    [(sci, "")] -> pure (sci :: Scientific)
    _ -> fail "invalid number"

-- 小数点以下ナノ秒を 0..999,999,999 へ丸めずに桁合わせ（切詰め/pad）
-- 入力は "360_227_400" などアンダースコア入りでもOK
pFracNanos :: Parser Int
pFracNanos = do
  _ <- char '.'
  ds <- some (satisfy (\c -> isDigit c || c == '_')) -- sus
  let raw = filter isDigit ds
      take9 = take 9 raw
      padded = take9 <> replicate (9 - length take9) '0'
  pure (read (if null padded then "0" else padded))

-- 単位 *順序大事*（ms/us/ns を先）
pTimeUnit :: Parser Text
pTimeUnit =
  choice
    [ string' "ms",
      string' "us",
      string' "ns",
      string' "d",
      string' "h",
      string' "m",
      string' "s"
    ]

-- Scientific × 単位 を 1 チャンクとして読む（"14.7s" など）
pDurChunk :: Parser (Scientific, Text)
pDurChunk = do
  n <- pNumberSci
  u <- pTimeUnit
  pure (n, T.toLower u)

toNS :: (Scientific, Text) -> Integer
toNS (sci, u) =
  let factor :: Integer
      factor = case u of
        "d" -> 86400 * 1000000000
        "h" -> 3600 * 1000000000
        "m" -> 60 * 1000000000
        "s" -> 1000000000
        "ms" -> 1000000
        "us" -> 1000
        "ns" -> 1
        _ -> 0
      rat = toRational sci
   in round (rat * fromIntegral factor)

-- T#... / TIME#... をパース
pTimeLit :: Parser Expr
pTimeLit = lexeme $ do
  _ <- try (string' "TIME#") <|> string' "T#"
  (dur, neg) <- pDurationSegments
  pure $ ETIME (if neg then negate dur else dur)

pDateLit :: Parser Expr
pDateLit = lexeme $ do
  _ <- try (string' "DATE#") <|> string' "D#"
  EDATE <$> pDateYMD

pTODLit :: Parser Expr
pTODLit = lexeme $ do
  _ <- try (string' "TIME_OF_DAY#") <|> string' "TOD#"
  ETOD <$> pHMSWithFrac

pDTLit :: Parser Expr
pDTLit = lexeme $ do
  _ <- try (string' "DATE_AND_TIME#") <|> string' "DT#"
  d <- pDateYMD
  _ <- char '-'
  EDT . DateTime d <$> pHMSWithFrac

pLTimeLit :: Parser Expr
pLTimeLit = lexeme $ do
  _ <- try (string' "LTIME#") <|> string' "LT#"
  (dur, neg) <- pDurationSegments
  pure $ ELTIME (if neg then negate dur else dur)

pLDateLit :: Parser Expr
pLDateLit = lexeme $ do
  _ <- try (string' "LDATE#") <|> string' "LD#"
  ELDATE <$> pDateYMD

pLTODLit :: Parser Expr
pLTODLit = lexeme $ do
  _ <- try (string' "LTIME_OF_DAY#") <|> string' "LTOD#"
  ELTOD <$> pHMSWithFrac

pLDTLit :: Parser Expr
pLDTLit = lexeme $ do
  _ <- try (string' "LDATE_AND_TIME#") <|> string' "LDT#"
  d <- pDateYMD
  _ <- char '-'
  ELDT . DateTime d <$> pHMSWithFrac

pDurationSegments :: Parser (Integer, Bool)
pDurationSegments = do
  neg <- optional (char '-')
  ch1 <- pDurChunk
  rest <- many (optional (char '_') *> pDurChunk)
  let total = sum (map toNS (ch1 : rest))
  pure (total, isJust neg)

pDateYMD :: Parser Date
pDateYMD = do
  y <- read <$> some digitChar
  _ <- char '-'
  m <- read <$> some digitChar
  _ <- char '-'
  d <- read <$> some digitChar
  pure (Date y m d)

pHMSWithFrac :: Parser TimeOfDay
pHMSWithFrac = do
  hh <- read <$> some digitChar
  _ <- char ':'
  mm <- read <$> some digitChar
  _ <- char ':'
  ss <- read <$> some digitChar
  nanos <- fromMaybe 0 <$> optional pFracNanos
  pure (TimeOfDay hh mm ss nanos)

-- 引数リスト: ( ... ) 部分だけ
-- 例:
--   f(1, 2)
--   f(x := 1, y := 2)
--   f(1, y := 2)
pCallArgs :: Parser [CallArg]
pCallArgs =
  parens (pArg `sepBy` symbol ",")
  where
    -- 名前付き: ident := expr
    pArg =
      try namedArg <|> posArg

    namedArg = do
      name <- identifier
      _ <- symbol ":="
      CallArgNamed name <$> pExpr

    posArg =
      CallArgPos <$> pExpr
