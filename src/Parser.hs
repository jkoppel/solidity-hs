{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (chr)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Identifiers
import Parser.Reserved (reservedKeywords, yulEvmBuiltin)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)
import Types
import Yul

data ParserError
  = ParserError Text
  | ReservedKeyword Text
  | ContainsAssembly
  | NonEmptyPath
  deriving stock (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError msg) = T.unpack msg
  showErrorComponent (ReservedKeyword msg) = T.unpack msg <> " is a reserved keyword"
  showErrorComponent ContainsAssembly = "Solidity program contains 'assembly' statement"
  showErrorComponent NonEmptyPath = "Non-empty path was empty"

type Parser = Parsec ParserError Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword word = lexeme (string word)

semi :: Parser Text
semi = symbol ";"

comma :: Parser Text
comma = keyword ","

parseIdentifier :: Parser Identifier
parseIdentifier = Identifier <$> lexeme (choice [ident, string "from", string "error", string "revert"])
  where
    extra = satisfy (`elem` ['_', '$'])
    ident = do
      start <- T.pack <$> some (letterChar <|> extra)
      end <- T.pack <$> many (alphaNumChar <|> extra)
      let name = start <> end
      if name `elem` reservedKeywords
        then customFailure $ ReservedKeyword name
        else pure name

parseIdentifierPath :: Parser IdentifierPath
parseIdentifierPath = do
  path <- sepBy parseIdentifier (char '.')
  if null path
    then customFailure $ ParserError "empty identifier path"
    else pure $ IdentifierPath path

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

unicodeEscape :: Parser Char
unicodeEscape = try $ do
    void $ string "\\u"
    code <- count 4 hexDigitChar
    pure $ chr $ read $ "0x" ++ code

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') (unicodeEscape <|> L.charLiteral)

consecutiveStrings :: Parser Text -> Parser Text
consecutiveStrings component = do
  firstComponent <- component
  rest <- many (try (sc *> component))
  pure $ mconcat $ firstComponent : rest

-- NOTE Oct 2, 2024: Still many differences between L.charLiteral and Solidity chars.
--                   E.g.: \x takes two digits in Solidity, but many more in L.charLiteral

genStringLiteral :: Char -> Parser Text
genStringLiteral quote = T.pack <$> (char quote *> manyTill (unicodeEscape <|> L.charLiteral) (char quote))

stringLiteral' :: Parser Text
stringLiteral' = genStringLiteral '\''

stringLiteral :: Parser Text
stringLiteral = genStringLiteral '\"'


genHexStringLiteral :: Char -> Parser Text
genHexStringLiteral quote = T.pack <$> (keyword "hex" *> char quote *> manyTill L.charLiteral (char quote))

hexStringLiteral' :: Parser Text
hexStringLiteral' = genHexStringLiteral '\''

hexStringLiteral :: Parser Text
hexStringLiteral = genHexStringLiteral '\"'

genUnicodeStringLiteral :: Char -> Parser Text
genUnicodeStringLiteral quote = T.pack <$> (keyword "unicode" *> char quote *> manyTill L.charLiteral (char quote))

unicodeStringLiteral' :: Parser Text
unicodeStringLiteral' = genUnicodeStringLiteral '\''

unicodeStringLiteral :: Parser Text
unicodeStringLiteral = genUnicodeStringLiteral '\"'

parsePragma :: Parser PragmaDefinition
parsePragma = keyword "pragma" *> takeWhile1P Nothing (/= ';') <* semi >>= pure <$> PragmaDefinition <?> "Pragma"

parseImport :: Parser ImportDefinition
parseImport = keyword "import" *> choice [parseImportPath, parseSymbolAliases, parseWildcard] <* semi >>= pure <$> ImportDefinition <?> "Import"
  where
    parseImportPath = do
      path <- parsePath
      ident <- try $ optional (keyword "as" *> parseIdentifier)
      pure $ ImportPath path ident
    parseSymbolAliases = do
      symbols <- braces (sepEndBy parseSymbolAlias comma)
      path <- keyword "from" *> parsePath
      pure $ ImportAliases symbols path
    parseSymbolAlias = do
      ident <- parseIdentifier
      alias <- try $ optional (keyword "as" *> parseIdentifier)
      pure (ident, alias)
    parseWildcard = do
      ident <- keyword "*" *> keyword "as" *> parseIdentifier
      void (keyword "from")
      ImportWildcard ident <$> parsePath

parsePath :: Parser Text
parsePath = do
  text <- choice [stringLiteral, stringLiteral']
  if T.null text
    then customFailure NonEmptyPath
    else text <$ sc

parseInheritance :: Parser InheritanceSpecifier
parseInheritance = do
  ident <- parseIdentifierPath
  args <- try $ optional parseCallArgumentList
  pure $ InheritanceSpecifier ident args

parseContract :: Parser ContractDefinition
parseContract = do
  abstract <- isJust <$> (optional (try $ keyword "abstract") <?> "abstract")
  void (keyword "contract") <?> "contract"
  name <- parseIdentifier <?> "name"
  inheritance <- try . optional $ keyword "is" *> sepEndBy parseInheritance comma
  body <- braces parseContractBody
  pure $ ContractDefinition {abstract, name, inheritance, body}

parseInterface :: Parser InterfaceDefinition
parseInterface = do
  void (keyword "interface") <?> "interface"
  name <- parseIdentifier <?> "identifier"
  inheritance <- try . optional $ keyword "is" *> sepEndBy parseInheritance comma
  body <- braces parseContractBody
  pure $ InterfaceDefinition {name, inheritance, body}

parseLibrary :: Parser LibraryDefinition
parseLibrary = do
  void (keyword "library") <?> "library"
  name <- parseIdentifier <?> "identifier"
  body <- braces parseContractBody
  pure $ LibraryDefinition {name, body}

parseParameterList :: Parser [Parameter]
parseParameterList = sepEndBy parseParameter comma

parseFunction :: Parser FunctionDefinition
parseFunction = do
  void (keyword "function") <?> "function"
  name <- choice [parseIdentifier, Identifier <$> keyword "fallback", Identifier <$> keyword "receive"] <?> "function name"
  params <- parens parseParameterList <?> "function params"
  restrictions <- parseFunctionRestriction <?> "function restrictions"
  returns <- optional (keyword "returns" *> parens parseParameterList <?> "function returns")
  body <- semi *> (BlockStatement <$> mempty) <|> parseBlock
  pure $ FunctionDefinition {name, params, restrictions, returns, body}


parseFunctionType :: Parser FunctionType
parseFunctionType = do
  void (keyword "function") <?> "function"
  params <- parens parseParameterList <?> "function params"
  restrictions <- parseFunctionRestriction <?> "function restrictions"
  returns <- optional (keyword "returns" *> parens parseParameterList <?> "function returns")
  pure $ MkFunctionType {params, restrictions, returns}

parseFallbackFunction :: Parser FallbackFunctionDefinition
parseFallbackFunction = do
  void (keyword "fallback") <?> "fallback func"
  params <- parens parseParameterList <?> "fallback params"
  restrictions <- parseFunctionRestriction <?> "function restrictions"
  returns <- (optional . try) (keyword "returns" *> parens parseParameterList) <?> "function returns"
  body <- semi *> (BlockStatement <$> mempty) <|> parseBlock
  pure $ FallbackFunctionDefinition {params, restrictions, returns, body}

parseReceiveFunction :: Parser ReceiveFunctionDefinition
parseReceiveFunction = do
  void (keyword "receive") <?> "receive func"
  void (parens sc) <?> "receive parens"
  restrictions <- parseFunctionRestriction <?> "receive restrictions"
  body <- semi *> (BlockStatement <$> mempty) <|> parseBlock
  pure $ ReceiveFunctionDefinition {restrictions, body}

parseUsingAlias :: Parser UsingDirectiveAlias
parseUsingAlias = do
  ident <- parseIdentifierPath
  op <- optional $ do
    void $ keyword "as"
    choice [
      symbol "~" >> pure (AUnaryOp UPreBitNot),
      symbol "&" >> pure (ABinaryOp BitAnd),
      symbol "|" >> pure (ABinaryOp BitOr),
      symbol "^" >> pure (ABinaryOp BitXor),
      symbol "+" >> pure (ABinaryOp Add),
      symbol "/" >> pure (ABinaryOp Div),
      symbol "%" >> pure (ABinaryOp Mod),
      symbol "*" >> pure (ABinaryOp Mul),
      symbol "-" >> pure (ABinaryOp Sub),
      symbol "==" >> pure (ABinaryOp Equal),
      symbol ">=" >> pure (ABinaryOp GreaterEqual),
      symbol ">" >> pure (ABinaryOp GreaterThan),
      symbol "<=" >> pure (ABinaryOp LessEqual),
      symbol "<" >> pure (ABinaryOp LessThan),
      symbol "!=" >> pure (ABinaryOp NotEqual)
     ]
  pure $ UsingDirectiveAlias {ident, op}

parseUsingDirective :: Parser UsingDirective
parseUsingDirective = do
  void (keyword "using") <?> "using"
  binders <- choice [
    UsingDirectiveAliases <$> braces (sepBy parseUsingAlias comma),
    UsingDirectiveBoundIdent <$> parseIdentifierPath <?> "using path"
   ]
  void $ keyword "for"
  bound <- choice [DirectiveAll <$ "*", DirectiveType <$> parseTypeName] <?> "using bound"
  global <- isJust <$> optional (keyword "global")
  void semi
  pure UsingDirective {binders, bound, global}

parseErrorDefinition :: Parser ErrorDefinition
parseErrorDefinition = do
  void (keyword "error") <?> "error"
  ident <- parseIdentifier <?> "error ident"
  params <- parens (sepEndBy parseErrorParameter comma) <?> "error params"
  void semi
  pure ErrorDefinition {ident, params}

parseErrorParameter :: Parser (TypeName, Maybe Identifier)
parseErrorParameter = do
  kind <- parseTypeName
  ident <- optional parseIdentifier
  pure (kind, ident)

parseEvent :: Parser EventDefinition
parseEvent = do
  void (keyword "event") <?> "event"
  ident <- parseIdentifier <?> "event ident"
  params <- parens (sepEndBy parseEventParameter comma)
  anonymous <- isJust <$> optional (keyword "anonymous")
  void semi
  pure EventDefinition {ident, params, anonymous}

parseEventParameter :: Parser (TypeName, Maybe (), Maybe Identifier)
parseEventParameter = do
  kind <- parseTypeName
  indexed <- optional $ void (keyword "indexed")
  ident <- optional parseIdentifier
  pure (kind, indexed, ident)

parseParameter :: Parser Parameter
parseParameter = do
  kind <- parseTypeName <* sc
  location <- optional parseDataLocation
  ident <- optional parseIdentifier
  pure $ Parameter {kind, location, ident}

parseStruct :: Parser StructDefinition
parseStruct = do
  void (keyword "struct") <?> "struct"
  ident <- parseIdentifier
  members <- braces (many parseStructMember)
  pure $ StructDefinition {ident, members}

parseStructMember :: Parser (TypeName, Identifier)
parseStructMember = do
  kind <- parseTypeName
  ident <- parseIdentifier
  void semi
  pure (kind, ident)

parseEnum :: Parser EnumDefinition
parseEnum = do
  void (keyword "enum") <?> "enum"
  ident <- parseIdentifier
  members <- braces (sepEndBy parseIdentifier comma)
  pure $ EnumDefinition {ident, members}

parseUserType :: Parser UserDefinedValueDefinition
parseUserType = do
  void (keyword "type") <?> "type"
  ident <- parseIdentifier
  void (keyword "is")
  kind <- parseElementaryTypeName
  void semi
  pure $ UserDefinedValueDefinition {ident, kind}

parseConstVar :: Parser ConstantVarDeclaration
parseConstVar = do
  kind <- parseTypeName <?> "const type"
  void (keyword "constant")
  ident <- parseIdentifier
  expr <- keyword "=" *> parseExpression
  void semi
  pure $ ConstantVarDeclaration {kind, ident, expr}

parseStateVariableDecl :: Parser StateVariableDec
parseStateVariableDecl = do
  kind <- parseTypeName <?> "state variable type"
  modifiers <- optional $ many parseStateVariableModifier
  ident <- parseIdentifier
  expr <- optional (keyword "=" *> parseExpression)
  void semi
  pure $ StateVariableDec {kind, modifiers, ident, expr}

parseStateVariableModifier :: Parser StateVariableModifier
parseStateVariableModifier =
  choice
    [ StatePublic <$ keyword "public",
      StatePrivate <$ keyword "private",
      StateInternal <$ keyword "internal",
      StateConstant <$ keyword "constant",
      StateOverride <$> parseOverrideSpecifier,
      StateImmutable <$ keyword "immutable"
    ]

parseOverrideSpecifier :: Parser OverrideSpecifier
parseOverrideSpecifier = do
  void (keyword "override") <?> "override"
  path <- optional $ parens (sepEndBy parseIdentifierPath comma)
  pure $ OverrideSpecifier {path}

parseDataLocation :: Parser DataLocation
parseDataLocation = choice [Memory <$ keyword "memory", Storage <$ keyword "storage", Calldata <$ keyword "calldata"]

parseFunctionRestriction :: Parser [FunctionRestriction]
parseFunctionRestriction =
  (many . choice)
    [ FuncVisibility <$> parseVisibility,
      FuncMutability <$> parseMutability,
      FuncVirtual <$ keyword "virtual",
      FuncOverride <$> parseOverrideSpecifier,
      FuncModifierInvocation <$> try parseModifierInvocation
    ]

parseModifierDefinition :: Parser ModifierDefinition
parseModifierDefinition = do
  void (keyword "modifier") <?> "modifier"
  ident <- parseIdentifier
  params <- optional (parens (sepEndBy parseParameter comma)) <?> "function params"
  modifiers <- parseModifierModifier
  body <- semi *> (BlockStatement <$> mempty) <|> parseBlock
  pure $ ModifierDefinition {ident, params, modifiers, body}

parseModifierModifier :: Parser [ModifierModifier]
parseModifierModifier = (many . choice) [ModVirtual <$ keyword "virtual", ModOverride <$> parseOverrideSpecifier]

parseWhile :: Parser WhileStatement
parseWhile = do
  void (keyword "while") <?> "while"
  expr <- parens parseExpression
  stmt <- parseStatement
  pure $ WhileStatement {expr, stmt}

parseDoWhile :: Parser DoWhileStatement
parseDoWhile = do
  void (keyword "do") <?> "do"
  stmt <- parseStatement
  void (keyword "while") <?> "while"
  expr <- parens parseExpression <* semi
  pure $ DoWhileStatement {stmt, expr}

parseIf :: Parser IfStatement
parseIf = do
  void (keyword "if") <?> "if"
  expr <- parens parseExpression
  trueStmt <- parseStatement
  elseStmt <- try . optional $ keyword "else" *> parseStatement
  pure $ IfStatement {expr, trueStmt, elseStmt}

parseForStatement :: Parser ForStatement
parseForStatement = do
  void (keyword "for") <?> "for"
  (initializer, condition, increment) <- parens parseForStart
  body <- parseStatement
  pure $ ForStatement {initializer, condition, increment, body}

parseForStart :: Parser (ForInitializer, Maybe Expression, Maybe Expression)
parseForStart = do
  initializer <- parseForInitializer
  condition <- optional parseExpression <* semi
  increment <- optional parseExpression
  pure (initializer, condition, increment)

parseForInitializer :: Parser ForInitializer
parseForInitializer =
  choice
    [ ForInitializerVariable <$> try parseVariableDecStatement,
      ForInitializerExpression <$> try parseExpression <* semi,
      ForInitializerEmpty <$ semi
    ]

parseVariableStatement :: Parser VariableDeclarationStatement
parseVariableStatement = choice [parseVariableDecStatement, parseVariableDecTupleStatement]

parseVariableDecStatement :: Parser VariableDeclarationStatement
parseVariableDecStatement = do
  dec <- parseVariableDeclaration
  expr <- optional (keyword "=" *> parseExpression)
  void semi
  pure $ VariableDecStatement dec expr

parseVariableDecTupleStatement :: Parser VariableDeclarationStatement
parseVariableDecTupleStatement = do
  decs <- parens (sepEndBy (optional parseVariableDeclaration) comma)
  void (keyword "=")
  expr <- parseExpression
  void semi
  pure $ VariableDecTupleStatement decs expr

parseVariableDeclaration :: Parser VariableDeclaration
parseVariableDeclaration = do
  kind <- parseTypeName
  location <- optional parseDataLocation
  ident <- parseIdentifier
  pure $ VariableDeclaration {kind, location, ident}

parseExpressionStatement :: Parser Statement
parseExpressionStatement = ExpressionStatement <$> parseExpression <* semi

parseTryStatement :: Parser TryStatement
parseTryStatement = do
  void (keyword "try") <?> "try"
  expr <- parseExpr
  ret <- try $ optional (keyword "returns" *> parens parseParameterList)
  body <- parseBlock
  catch <- some parseCatchClause
  pure $ TryStatement {expr, ret, body, catch}
  where
    parseExpr = makeExprParser parseExpression' parseTbl <* sc
    parseTbl = [Postfix $ foldr1 (flip (.)) <$> some (parseIndex <|> parseSlice <|> parseMemberAccess <|> parseFunctionCall)] : tail parseTable

parseCatchClause :: Parser CatchClause
parseCatchClause = do
  void (keyword "catch") <?> "catch"
  ident <- try $ optional parseIdentifier
  params <- try $ optional $ parens parseParameterList
  body <- parseBlock
  pure $ CatchClause {ident, params, body}

parseEmitStatement :: Parser EmitStatement
parseEmitStatement = do
  void (keyword "emit") <?> "emit"
  expr <- parseExpression <* semi
  pure $ EmitStatement {expr}

parseRevertStatement :: Parser RevertStatement
parseRevertStatement = do
  void (keyword "revert") <?> "revert"
  expr <- parseExpression <* semi
  pure $ RevertStatement {expr}

parseAssembly :: Parser AssemblyStatement
parseAssembly = do
  void (keyword "assembly") <?> "assembly"
  dialect <- try (optional stringLiteral) <* sc
  flags <- choice [parens (sepEndBy stringLiteral comma), mempty]
  body <- braces $ many parseYulStatement
  pure $ AssemblyStatement {dialect, flags, body}

parseYulIdentifier :: Parser YulIdentifier
parseYulIdentifier = do
  name <- ident <* sc
  pure $
    if name `elem` yulEvmBuiltin
      then YulEvmBuiltin (Identifier name)
      else YulIdentifier (Identifier name)
  where
    extra = satisfy (`elem` ['_', '$'])
    ident = do
      start <- T.pack <$> some (letterChar <|> extra)
      end <- T.pack <$> many (alphaNumChar <|> extra)
      pure $ start <> end

parseYulIdentifierPath :: Parser YulIdentifierPath
parseYulIdentifierPath = do
  path <- sepBy parseYulIdentifier (char '.')
  if null path
    then customFailure $ ParserError "empty identifier path"
    else pure $ YulIdentifierPath path

parseYulVariableDeclaration :: Parser YulVariableDeclaration
parseYulVariableDeclaration = do
  void (keyword "let") <?> "yul variable"
  choice [simple, multi]
  where
    simple = do
      ident <- parseYulIdentifier
      void (keyword ":=")
      YulVariableDeclaration ident <$> parseYulExpression
    multi = do
      idents <- sepEndBy parseYulIdentifier comma
      void (keyword ":=")
      YulMultipleVariableDeclaration idents <$> parseYulFunctionCall

parseYulAssignmentDeclaration :: Parser YulAssignmentDeclaration
parseYulAssignmentDeclaration = choice [simple, multi]
  where
    simple = do
      path <- parseYulIdentifierPath
      void (keyword ":=")
      YulAssignmentDeclaration path <$> parseYulExpression
    multi = do
      paths <- sepEndBy parseYulIdentifierPath comma
      void (keyword ":=")
      YulMultipleAssignmentDeclaration paths <$> parseYulFunctionCall

parseYulBlock :: Parser YulStatement
parseYulBlock = braces $ many parseYulStatement >>= pure <$> YulBlock

parseYulStatement :: Parser YulStatement
parseYulStatement =
  choice
    ( map
        try
        [ parseYulBlock,
          YulVariableStatement <$> parseYulVariableDeclaration,
          YulAssignment <$> parseYulAssignmentDeclaration,
          YulFunctionCall <$> parseYulFunctionCall,
          YulIfStatement <$> parseYulIf,
          YulForStatement <$> parseYulFor,
          YulSwitchStatement <$> parseYulSwitch,
          YulLeave <$ keyword "leave",
          YulBreak <$ keyword "break",
          YulContinue <$ keyword "continue",
          YulFunctionDefinition <$> parseYulFunctionDefinition
        ]
    )
    <* sc

parseYulFunctionCall :: Parser YulFunctionCallDeclaration
parseYulFunctionCall = do
  ident <- parseYulIdentifier
  body <- parens (sepEndBy parseYulExpression comma)
  pure $ YulFunctionCallDeclaration {ident, body}

parseYulIf :: Parser YulIfStatementDeclaration
parseYulIf = do
  void (keyword "if") <?> "yul if"
  expr <- parseYulExpression
  body <- parseYulBlock
  pure $ YulIfStatementDeclaration {expr, body}

parseYulFor :: Parser YulForStatementDeclaration
parseYulFor = do
  void (keyword "for" <* sc) <?> "yul for"
  initializer <- parseYulBlock
  condition <- parseYulExpression
  increment <- parseYulBlock
  body <- parseYulBlock
  pure $ YulForStatementDeclaration {initializer, condition, increment, body}

parseYulSwitch :: Parser YulSwitchStatementDeclaration
parseYulSwitch = do
  void (keyword "switch" <* sc) <?> "yul switch"
  expr <- parseYulExpression
  cases <- many switchCase
  defaultBlock <- try . optional $ defaultCase
  pure $ YulSwitchStatementDeclaration {expr, cases, defaultBlock}
  where
    switchCase = do
      void (keyword "case") <?> "yul case"
      ident <- parseYulLiteral
      body <- parseYulBlock
      pure (ident, body)
    defaultCase = do
      void (keyword "default") <?> "yul default"
      parseYulBlock

parseYulFunctionDefinition :: Parser YulFunctionDefinitionDeclaration
parseYulFunctionDefinition = do
  void (keyword "function") <?> "yul function"
  ident <- parseYulIdentifier
  params <- parens (sepEndBy parseYulIdentifier comma)
  returns <- try $ optional (keyword "->" >> sepEndBy parseYulIdentifier comma)
  body <- parseYulBlock
  pure $ YulFunctionDefinitionDeclaration {ident, params, returns, body}

parseYulExpression :: Parser YulExpression
parseYulExpression =
  choice
    [ YulExpressionLiteral <$> parseYulLiteral,
      YulExpressionPath <$> try (parseYulIdentifierPath <* notFollowedBy "("),
      YulExpressionFunctionCall <$> parseYulFunctionCall
    ]

parseYulLiteral :: Parser YulLiteral
parseYulLiteral =
  choice
    [ YulHexString . T.pack <$> try (keyword "hex" >> char '"' >> manyTill L.charLiteral (char '"')),
      YulHexString . T.pack <$> try (keyword "hex" >> char '\'' >> manyTill L.charLiteral (char '\'')),
      YulStringLiteral <$> stringLiteral,
      YulHexNumber <$> lexeme (chunk "0x" >> L.hexadecimal),
      YulDecimalNumber <$> L.decimal,
      YulBoolean True <$ keyword "true",
      YulBoolean False <$ keyword "false"
    ]
    <* sc

parseBlock :: Parser Statement
parseBlock = do
  stmt <- braces $ (many . choice) [parseStatement, keyword "unchecked" *> parseBlock]
  pure $ BlockStatement stmt

parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseBlock,
      VariableStatement <$> try parseVariableStatement,
      try parseExpressionStatement,
      If <$> parseIf,
      For <$> parseForStatement,
      While <$> parseWhile,
      DoWhile <$> parseDoWhile,
      Continue <$ (keyword "continue" <* semi),
      Break <$ (keyword "break" <* semi),
      Try <$> parseTryStatement,
      Return <$> (keyword "return" *> optional parseExpression <* semi),
      Emit <$> parseEmitStatement,
      Revert <$> parseRevertStatement,
      Assembly <$> parseAssembly
    ]
    <* sc

parseContractBody :: Parser [ContractBodyElement]
parseContractBody =
  (many . choice)
    [ Constructor <$> parseConstructor,
      CFunction <$> parseFunction,
      CModifier <$> parseModifierDefinition,
      CFallbackFunction <$> parseFallbackFunction,
      CReceiveFunction <$> parseReceiveFunction,
      CStruct <$> parseStruct,
      CEnum <$> parseEnum,
      CUserDefinedType <$> parseUserType,
      CStateVariableDec <$> try parseStateVariableDecl,
      CEvent <$> parseEvent,
      CError <$> parseErrorDefinition,
      CUsing <$> parseUsingDirective
    ]
    <* sc

parseConstructor :: Parser ConstructorDefinition
parseConstructor = do
  void (keyword "constructor") <?> "constructor"
  params <- parens (sepEndBy parseParameter comma) <?> "constructor params"
  modifiers <- optional parseConstructorModifier
  body <- parseBlock
  pure $ ConstructorDefinition {params, modifiers, body}

parseConstructorModifier :: Parser [ConstructorModifier]
parseConstructorModifier =
  (many . choice)
    [ ConModifierInvocation <$> try parseModifierInvocation,
      ConPayable <$ keyword "payable",
      ConInternal <$ keyword "internal",
      ConPublic <$ keyword "public"
    ]

parseModifierInvocation :: Parser ModifierInvocation
parseModifierInvocation = do
  path <- parseIdentifierPath
  args <- optional parseCallArgumentList
  pure $ ModifierInvocation {path, args}

parseMutability :: Parser StateMutability
parseMutability =
  choice
    [ Pure <$ keyword "pure",
      View <$ keyword "view",
      Payable <$ keyword "payable"
    ]

parseVisibility :: Parser FunctionVisibility
parseVisibility =
  choice
    [ FuncInternal <$ keyword "internal",
      FuncExternal <$ keyword "external",
      FuncPrivate <$ keyword "private",
      FuncPublic <$ keyword "public"
    ]

parseNumberUnit :: Parser NumberUnit
parseNumberUnit =
  choice
    [ Wei <$ keyword "wei",
      Gwei <$ keyword "gwei",
      Ether <$ keyword "ether",
      Seconds <$ keyword "seconds",
      Minutes <$ keyword "minutes",
      Hours <$ keyword "hours",
      Days <$ keyword "days",
      Weeks <$ keyword "weeks",
      Years <$ keyword "years"
    ]
    <* sc

parseElementaryTypeName :: Parser ElementaryTypeName
parseElementaryTypeName =
  choice
    [ AddressPayable <$ try (keyword "address" <* keyword "payable"),
      Address <$ try (keyword "address"),
      Bool <$ keyword "bool",
      String <$ keyword "string",
      FixedBytes <$> try (keyword "bytes" *> L.decimal <* space),
      Bytes <$ try (keyword "bytes" <* space),
      SignedInteger <$> (keyword "int" *> optional L.decimal),
      UnsignedInteger <$> (keyword "uint" *> optional L.decimal),
      Fixed <$ keyword "fixed",
      Ufixed <$ keyword "ufixed"
    ]
    <* sc

parseLiteral :: Parser Literal
parseLiteral =
  choice
    [ BooleanLiteral True <$ keyword "true",
      BooleanLiteral False <$ keyword "false",
      StringLiteral <$> consecutiveStrings (try stringLiteral <|> try stringLiteral'),
      NumberLiteral <$> lexeme (chunk "0x" >> L.hexadecimal) <*> optional parseNumberUnit,
      parseNumberLiteral,
      HexStringLiteral <$> consecutiveStrings (try hexStringLiteral <|> try hexStringLiteral'),
      UnicodeStringLiteral <$> consecutiveStrings (try unicodeStringLiteral <|> try unicodeStringLiteral')
    ]
    <* sc

parseNumberLiteral :: Parser Literal
parseNumberLiteral = NumberLiteral <$> lexeme (choice [try parseDouble, parseInt]) <*> optional parseNumberUnit

parseInt :: Parser Double
parseInt = (checkNum . readMaybe) . mconcat =<< sepBy1 (some digitChar) (char '_')

parseDouble :: Parser Double
parseDouble = checkNum . readMaybe . addZero =<< mconcat [nums, dot <|> expo]
  where
    nums, dot, expo :: Parser String
    nums = concat <$> sepBy (some digitChar) (char '_')
    dot = mconcat [pure <$> char '.', nums, option "" expo]
    expo = mconcat [pure <$> (char 'e' <|> char 'E'), pure <$> option '+' (char '-'), nums]
    addZero n@('.' : _) = "0" <> n
    addZero n = n

checkNum :: Maybe Double -> Parser Double
checkNum = maybe (fail "Not a double") pure

parseTypeNameGen :: Bool -> Parser TypeName
parseTypeNameGen mandatoryArrayType = do
  typ <- parseType
  arr <- if mandatoryArrayType
          then Just <$> some parseArrayTypeStatusNonExpression
          else optional $ some parseArrayTypeStatus
  pure $ case arr of
    Just expr -> ArrayType typ expr
    Nothing -> typ
  where
    parseType =
      choice
        [ ElementaryType <$> parseElementaryTypeName,
          FunctionType <$> parseFunctionType,
          MappingType <$> parseMappingType,
          IdentifierType <$> try parseIdentifierPath
        ]
        
    parseArrayTypeStatus :: Parser ArrayTypeStatus
    parseArrayTypeStatus =
      brackets $
        choice
          [ ArrayTypeExpression <$> parseExpression,
            ArrayTypeEmpty <$ sc
          ]

    parseArrayTypeStatusNonExpression :: Parser ArrayTypeStatus
    parseArrayTypeStatusNonExpression = brackets $ ArrayTypeEmpty <$ sc

parseTypeName :: Parser TypeName
parseTypeName = parseTypeNameGen False

parseTypeNameNonExpression :: Parser TypeName
parseTypeNameNonExpression = parseTypeNameGen True

parseMappingType :: Parser MappingDefinition
parseMappingType = do
  void (keyword "mapping" *> symbol "(")
  mapping <- parseMappingKey
  void (symbol "=>")
  kind <- parseTypeName
  kindIdentifier <- optional parseIdentifier
  void (keyword ")")
  pure $ MappingDefinition {mapping, kind, kindIdentifier}
  where
    parseMappingKey =
      choice
        [ MappingKey <$> (MappingElementaryType <$> parseElementaryTypeName) <*> optional parseIdentifier,
          MappingKey <$> (MappingIdentifier <$> parseIdentifierPath) <*> optional parseIdentifier
        ]
        <* sc

parseCallArgumentList :: Parser CallArgument
parseCallArgumentList =
  parens $
    choice
      [ NamedArguments <$> braces (sepEndBy (parseColonSeparated parseIdentifier parseExpression) comma),
        CommaArguments <$> sepEndBy parseExpression comma
      ]

parseExpression :: Parser Expression
parseExpression = makeExprParser parseExpression' parseTable <* sc

parseExpression' :: Parser Expression
parseExpression' = do
  choice
    [ PayableConversion <$> try (keyword "payable" *> parseCallArgumentList),
      MetaType <$> try (keyword "type" *> parens parseTypeName),
      NewType <$> try (keyword "new" *> parseTypeName),
      TupleExpression <$> parens (sepEndBy (optional parseExpression) comma),
      InlineArrayExpression <$> brackets (sepEndBy parseExpression comma),
      TypeExpression <$> try parseTypeNameNonExpression, -- This exists for types like A.B[], where otherwise greedy parsing would match parseIdentifier first and be unable to backtrack
      IdentifierExpression <$> try parseIdentifier,
      ExpressionLiteral <$> parseLiteral,
      TypeExpression <$> parseTypeName
    ]
    <* sc

parseColonSeparated :: Parser a -> Parser b -> Parser (a, b)
parseColonSeparated pA pB = pA >>= \a -> keyword ":" *> pB <* sc >>= \b -> pure (a, b)

parseIndex :: Parser (Expression -> Expression)
parseIndex = flip IndexExpression <$> brackets parseExpression

parseSlice :: Parser (Expression -> Expression)
parseSlice = try $ brackets $ do
  start <- try $ optional parseExpression
  _ <- keyword ":"
  end <- try $ optional parseExpression
  return $ \e -> SliceExpression e start end

parseMemberAccess :: Parser (Expression -> Expression)
parseMemberAccess = flip MemberAccess <$> (keyword "." *> parseAccess)
  where
    parseAccess = choice [MemberAccessIdentifier <$> parseIdentifier, MemberAccessAddress <$ keyword "address"]

parseFunctionCallOptions :: Parser (Expression -> Expression)
parseFunctionCallOptions = flip FunctionCallOptions <$> opts
 where
    opts = NamedArguments <$> braces (sepEndBy (parseColonSeparated parseIdentifier parseExpression) comma)

parseFunctionCall :: Parser (Expression -> Expression)
parseFunctionCall = flip FunctionCall <$> parseCallArgumentList

-- Ensures that operators that are prefixes of others are handled correctly
prefixOp :: Text -> Parser Text
prefixOp n = (lexeme . try) (string n <* notFollowedBy (oneOf ['=', '>', '|', '<', '&', '/']))

binary, binaryR :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)

-- Similar to the above functions, but applies a parser insted of just a text token
binary', binaryR' :: Parser Text -> (a -> a -> a) -> Operator Parser a
binary' op f = InfixL (f <$ op)
binaryR' op f = InfixR (f <$ op)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

parseTable :: [[Operator Parser Expression]]
parseTable =
  [ [Postfix $ foldr1 (flip (.)) <$> some (parseSlice <|> parseIndex <|> parseMemberAccess <|> parseFunctionCallOptions <|> parseFunctionCall)],
    [ prefix "++" (UnaryExpression UPreInc),
      prefix "--" (UnaryExpression UPreDec),
      prefix "!" (UnaryExpression UPreNot),
      prefix "~" (UnaryExpression UPreBitNot),
      prefix "delete" (UnaryExpression UPreDelete),
      prefix "-" (UnaryExpression UPreSub)
    ],
    [ postfix "++" (UnaryExpression UPostInc),
      postfix "--" (UnaryExpression UPreInc)
    ],
    [binaryR "**" (BinaryExpression Exp)],
    [ binary' (prefixOp "*") (BinaryExpression Mul),
      binary' (prefixOp "/") (BinaryExpression Div),
      binary' (prefixOp "%") (BinaryExpression Mod)
    ],
    [ binary' (prefixOp "+") (BinaryExpression Add),
      binary' (prefixOp "-") (BinaryExpression Sub)
    ],
    [ binary' (prefixOp ">>>") (BinaryExpression Shr),
      binary' (prefixOp "<<") (BinaryExpression Shl),
      binary' (prefixOp ">>") (BinaryExpression Sar)
    ],
    [binary' (prefixOp "&") (BinaryExpression BitAnd)],
    [binary' (prefixOp "^") (BinaryExpression BitXor)],
    [binary' (prefixOp "|") (BinaryExpression BitOr)],
    [ binary' (prefixOp "<") (BinaryExpression LessThan),
      binary' (prefixOp ">") (BinaryExpression GreaterThan),
      binary "<=" (BinaryExpression LessEqual),
      binary ">=" (BinaryExpression GreaterEqual)
    ],
    [ binary "==" (BinaryExpression Equal),
      binary "!=" (BinaryExpression NotEqual)
    ],
    [binary "&&" (BinaryExpression And)],
    [binary "||" (BinaryExpression Or)],
    [ Postfix parseTernary ],
    [ binaryR "=" (BinaryExpression Assign),
      binaryR "|=" (BinaryExpression AssignBitOr),
      binaryR "^=" (BinaryExpression AssignBitXor),
      binaryR "&=" (BinaryExpression AssignBitAnd),
      binaryR "<<=" (BinaryExpression AssignShl),
      binaryR ">>=" (BinaryExpression AssignSar),
      binaryR ">>>=" (BinaryExpression AssignShr),
      binaryR "+=" (BinaryExpression AssignAdd),
      binaryR "-=" (BinaryExpression AssignSub),
      binaryR "*=" (BinaryExpression AssignMul),
      binaryR "/=" (BinaryExpression AssignDiv),
      binaryR "%=" (BinaryExpression AssignMod)
    ]
  ]

-- This exists so statements like `a < b ? x = 1 : x = 2;` are parsed correctly, even though
-- assignment is lower precedence than the ternary operator, and so `x = a < b ? 1 : 2;`
parseTernary :: Parser (Expression -> Expression)
parseTernary = do
    void $ symbol "?"
    trueExpr <- parseExpression
    void $ symbol ":"
    falseExpr <- parseExpression
    return $ \condition -> ConditionalExpression condition trueExpr falseExpr

parseSolidity :: Parser Solidity
parseSolidity = sc *> (Solidity <$> syntax) <* eof
  where
    syntax =
      (many . choice)
        [ Pragma <$> parsePragma,
          Import <$> parseImport,
          Using <$> parseUsingDirective,
          Contract <$> parseContract,
          Interface <$> parseInterface,
          Library <$> parseLibrary,
          Function <$> parseFunction,
          ConstVar <$> try parseConstVar,
          Struct <$> parseStruct,
          Enum <$> parseEnum,
          UserDefinedType <$> parseUserType,
          Error <$> parseErrorDefinition
        ]

parseFile :: FilePath -> Text -> Either ParserError Solidity
parseFile file input = case runParser parseSolidity file input of
  Right res -> Right res
  Left (ParseErrorBundle err _) ->
    Left $ case NE.head err of
      TrivialError {} -> ParserError "megaparsec internal failure"
      FancyError _ es -> case S.findMin es of
        ErrorFail _ -> ParserError "INTERNAL ERROR, `fail` called inside parser"
        ErrorIndentation {} -> ParserError "indentation failure, should not happen"
        ErrorCustom e -> e

parseFile' :: FilePath -> Text -> Either Text Solidity
parseFile' file input = case runParser parseSolidity file input of
  Right res -> Right res
  Left err -> Left $ T.pack $ errorBundlePretty err

parseInput :: Text -> Either ParserError Solidity
parseInput = parseFile "file"
