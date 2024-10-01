{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Identifiers
import Optics hiding (Empty)
import Yul (YulStatement)

newtype Solidity = Solidity [SourceUnit] deriving stock (Show, Read, Eq, Ord, Generic)

newtype PragmaDefinition = PragmaDefinition {pragma :: Text} deriving stock (Show, Read, Eq, Ord, Generic)

newtype ImportDefinition = ImportDefinition ImportType deriving stock (Show, Read, Eq, Ord, Generic)

data ImportType
  = ImportPath Text (Maybe Identifier)
  | ImportAliases [(Identifier, Maybe Identifier)] Text
  | ImportWildcard Identifier Text
  deriving stock (Show, Read, Eq, Ord, Generic)

data ContractDefinition = ContractDefinition
  { abstract :: Bool,
    name :: Identifier,
    inheritance :: Maybe [InheritanceSpecifier],
    body :: [ContractBodyElement]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data InheritanceSpecifier = InheritanceSpecifier IdentifierPath (Maybe CallArgument)
  deriving stock (Show, Read, Eq, Ord, Generic)

data ConstructorDefinition = ConstructorDefinition
  { params :: [Parameter],
    modifiers :: Maybe [ConstructorModifier],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ConstructorModifier
  = ConPayable
  | ConInternal
  | ConPublic
  | ConModifierInvocation ModifierInvocation
  deriving stock (Show, Read, Eq, Ord, Generic)

data InterfaceDefinition = InterfaceDefinition
  { name :: Identifier,
    inheritance :: Maybe [InheritanceSpecifier],
    body :: [ContractBodyElement]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data FunctionDefinition = FunctionDefinition
  { name :: Identifier,
    params :: [Parameter],
    restrictions :: [FunctionRestriction],
    returns :: Maybe [Parameter],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data FunctionRestriction
  = FuncVisibility FunctionVisibility
  | FuncMutability StateMutability
  | FuncOverride OverrideSpecifier
  | FuncVirtual
  | FuncModifierInvocation ModifierInvocation
  deriving stock (Show, Read, Eq, Ord, Generic)

data ModifierInvocation = ModifierInvocation
  { path :: IdentifierPath,
    args :: Maybe CallArgument
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data FallbackFunctionDefinition = FallbackFunctionDefinition
  { params :: [Parameter],
    restrictions :: [FunctionRestriction],
    returns :: Maybe [Parameter],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ReceiveFunctionDefinition = ReceiveFunctionDefinition
  { restrictions :: [FunctionRestriction],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data LibraryDefinition = LibraryDefinition
  { name :: Identifier,
    body :: [ContractBodyElement]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data UsingDirective = UsingDirective
  { ident :: IdentifierPath,
    bound :: DirectiveBind
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ConstantVarDeclaration = ConstantVarDeclaration
  { kind :: TypeName,
    ident :: Identifier,
    expr :: Expression
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data StructDefinition = StructDefinition
  { ident :: Identifier,
    members :: [(TypeName, Identifier)]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data EnumDefinition = EnumDefinition
  { ident :: Identifier,
    members :: [Identifier]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data UserDefinedValueDefinition = UserDefinedValueDefinition
  { ident :: Identifier,
    kind :: ElementaryTypeName
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data DirectiveBind = DirectiveType TypeName | DirectiveAll
  deriving stock (Show, Read, Eq, Ord, Generic)

data ErrorDefinition = ErrorDefinition
  { ident :: Identifier,
    params :: [(TypeName, Maybe Identifier)]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data EventDefinition = EventDefinition
  { ident :: Identifier,
    params :: [(TypeName, Maybe (), Maybe Identifier)],
    anonymous :: Bool
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data StateVariableDec = StateVariableDec
  { kind :: TypeName,
    modifiers :: Maybe [StateVariableModifier],
    ident :: Identifier,
    expr :: Maybe Expression
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data StateVariableModifier
  = StatePublic
  | StatePrivate
  | StateInternal
  | StateConstant
  | StateImmutable
  | StateOverride OverrideSpecifier
  deriving stock (Show, Read, Eq, Ord, Generic)

data ModifierDefinition = ModifierDefinition
  { ident :: Identifier,
    params :: Maybe [Parameter],
    modifiers :: [ModifierModifier],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ModifierModifier = ModVirtual | ModOverride OverrideSpecifier
  deriving stock (Show, Read, Eq, Ord, Generic)

data Parameter = Parameter
  { kind :: TypeName,
    location :: Maybe DataLocation,
    ident :: Maybe Identifier
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

newtype OverrideSpecifier = OverrideSpecifier {path :: Maybe [IdentifierPath]}
  deriving newtype (Show, Read, Eq, Ord, Generic)

data DataLocation = Memory | Storage | Calldata
  deriving stock (Show, Read, Eq, Ord, Generic)

showDataLocation :: DataLocation -> Text
showDataLocation Memory = "memory"
showDataLocation Storage = "storage"
showDataLocation Calldata = "calldata"

data FunctionVisibility = FuncInternal | FuncExternal | FuncPrivate | FuncPublic
  deriving stock (Show, Read, Eq, Ord, Generic)

data StateMutability = Pure | View | Payable
  deriving stock (Show, Read, Eq, Ord, Generic)

data NumberUnit = Wei | Gwei | Ether | Seconds | Minutes | Hours | Days | Weeks | Years
  deriving stock (Show, Read, Eq, Ord, Generic)

data SourceUnit
  = Pragma PragmaDefinition
  | Import ImportDefinition
  | Contract ContractDefinition
  | Interface InterfaceDefinition
  | Library LibraryDefinition
  | Function FunctionDefinition
  | ConstVar ConstantVarDeclaration
  | Struct StructDefinition
  | Enum EnumDefinition
  | UserDefinedType UserDefinedValueDefinition
  | Error ErrorDefinition
  deriving stock (Show, Read, Eq, Ord, Generic)

data ContractBodyElement
  = Constructor ConstructorDefinition
  | CFunction FunctionDefinition
  | CModifier ModifierDefinition
  | CFallbackFunction FallbackFunctionDefinition
  | CReceiveFunction ReceiveFunctionDefinition
  | CStruct StructDefinition
  | CEnum EnumDefinition
  | CUserDefinedType UserDefinedValueDefinition
  | CStateVariableDec StateVariableDec
  | CEvent EventDefinition
  | CError ErrorDefinition
  | CUsing UsingDirective
  deriving stock (Show, Read, Eq, Ord, Generic)

data WhileStatement = WhileStatement
  { expr :: Expression,
    stmt :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data DoWhileStatement = DoWhileStatement
  { stmt :: Statement,
    expr :: Expression
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data IfStatement = IfStatement
  { expr :: Expression,
    trueStmt :: Statement,
    elseStmt :: Maybe Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ForStatement = ForStatement
  { initializer :: ForInitializer,
    condition :: Maybe Expression,
    increment :: Maybe Expression,
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data ForInitializer
  = ForInitializerVariable VariableDeclarationStatement
  | ForInitializerExpression Expression
  | ForInitializerEmpty
  deriving stock (Show, Read, Eq, Ord, Generic)

data VariableDeclaration = VariableDeclaration
  { kind :: TypeName,
    location :: Maybe DataLocation,
    ident :: Identifier
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

showVariableDeclaration :: VariableDeclaration -> Text
showVariableDeclaration var =
  showTypeName (var ^. #kind)
    <> T.intercalate
      " "
      [ maybe "" showDataLocation (var ^. #location),
        showIdentifier (var ^. #ident)
      ]

data VariableDeclarationStatement
  = VariableDecStatement VariableDeclaration (Maybe Expression)
  | VariableDecTupleStatement [Maybe VariableDeclaration] Expression
  deriving stock (Show, Read, Eq, Ord, Generic)

data TryStatement = TryStatement
  { expr :: Expression,
    ret :: Maybe [Parameter],
    body :: Statement,
    catch :: [CatchClause]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data CatchClause = CatchClause
  { ident :: Maybe Identifier,
    params :: Maybe [Parameter],
    body :: Statement
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

newtype EmitStatement = EmitStatement {expr :: Expression}
  deriving newtype (Show, Read, Eq, Ord, Generic)

newtype RevertStatement = RevertStatement {expr :: Expression}
  deriving newtype (Show, Read, Eq, Ord, Generic)

data AssemblyStatement = AssemblyStatement
  { dialect :: Maybe Text,
    flags :: [Text],
    body :: [YulStatement]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data Statement
  = BlockStatement [Statement]
  | VariableStatement VariableDeclarationStatement
  | ExpressionStatement Expression
  | If IfStatement
  | For ForStatement
  | While WhileStatement
  | DoWhile DoWhileStatement
  | Continue
  | Break
  | Try TryStatement
  | Return (Maybe Expression)
  | Emit EmitStatement
  | Revert RevertStatement
  | Assembly AssemblyStatement
  deriving stock (Show, Read, Eq, Ord, Generic)

data CallArgument
  = CommaArguments [Expression]
  | NamedArguments [(Identifier, Expression)]
  deriving stock (Show, Read, Eq, Ord, Generic)

showCallArgument :: CallArgument -> Text
showCallArgument (CommaArguments exprs) = "(" <> T.intercalate "," (map showExpression exprs) <> ")"
showCallArgument (NamedArguments exprs) = "(" <> T.intercalate "," (map (\(i, e) -> showIdentifier i <> " : " <> showExpression e) exprs) <> ")"

data MemberAccessType
  = MemberAccessIdentifier Identifier
  | MemberAccessAddress
  deriving stock (Show, Read, Eq, Ord, Generic)

data Expression
  = IndexExpression Expression Expression
  | SliceExpression Expression (Maybe Expression) (Maybe Expression)
  | MemberAccess Expression MemberAccessType
  | FunctionCallOptions Expression CallArgument
  | FunctionCall Expression CallArgument
  | PayableConversion CallArgument
  | MetaType TypeName
  | UnaryExpression UnaryOp Expression
  | BinaryExpression BinaryOp Expression Expression
  | ConditionalExpression Expression Expression Expression -- right assoc
  | NewType TypeName
  | TupleExpression [Maybe Expression]
  | InlineArrayExpression [Expression]
  | IdentifierExpression Identifier
  | ExpressionLiteral Literal
  | ElementaryTypeExpression ElementaryTypeName
  | Empty -- only used in tuple expressions
  deriving stock (Show, Read, Eq, Ord, Generic)

showExpression :: Expression -> Text
showExpression (IndexExpression e1 e2) = showExpression e1 <> "[" <> showExpression e2 <> "]"
showExpression (SliceExpression e1 e2 e3) = showExpression e1 <> "[" <> maybe "" showExpression e2 <> ":" <> maybe "" showExpression e3 <> "]"
showExpression (MemberAccess expr access) = showExpression expr <> "." <> T.pack (show access)
showExpression (FunctionCallOptions expr call) = showExpression expr <> showCallArgument call
showExpression (FunctionCall expr call) = showExpression expr <> showCallArgument call
showExpression (PayableConversion call) = T.pack $ "payable " <> show call
showExpression (MetaType name) = T.pack $ "type (" <> show name <> ")"
showExpression (UnaryExpression op expr) = showUnaryOp op expr
showExpression (BinaryExpression op e1 e2) = showBinaryOp op e1 e2
showExpression (ConditionalExpression e1 e2 e3) = showExpression e1 <> " ? " <> showExpression e2 <> " : " <> showExpression e3
showExpression (NewType name) = T.pack $ "new " <> show name
showExpression (TupleExpression exprs) = "(" <> T.intercalate "," (map (maybe "" showExpression) exprs) <> ")"
showExpression (InlineArrayExpression exprs) = "[" <> T.intercalate "," (map showExpression exprs) <> "]"
showExpression (IdentifierExpression ident) = showIdentifier ident
showExpression (ExpressionLiteral lit) = showLiteral lit
showExpression (ElementaryTypeExpression e) = showElementayType e
showExpression Empty = ""

data UnaryOp
  = UPreInc -- ++expr
  | UPreDec -- --expr
  | UPreNot -- !expr
  | UPreBitNot -- ~expr
  | UPreDelete -- delete expr
  | UPreSub -- -expr
  | UPostInc -- expr++
  | UPostDec -- expr--
  deriving stock (Show, Read, Eq, Ord, Generic)

showUnaryOp :: UnaryOp -> Expression -> Text
showUnaryOp UPreInc expr = T.pack $ "++" <> show expr
showUnaryOp UPreDec expr = T.pack $ "--" <> show expr
showUnaryOp UPreNot expr = T.pack $ "!" <> show expr
showUnaryOp UPreBitNot expr = T.pack $ "~" <> show expr
showUnaryOp UPreDelete expr = T.pack $ "delete " <> show expr
showUnaryOp UPreSub expr = T.pack $ "-" <> show expr
showUnaryOp UPostInc expr = T.pack $ show expr <> "++"
showUnaryOp UPostDec expr = T.pack $ show expr <> "--"

data BinaryOp
  = Exp -- (right assoc) expr ** expr
  | Mul -- expr * expr
  | Div -- expr / expr
  | Mod -- expr % expr
  | Add -- expr + expr
  | Sub -- expr - expr
  | Shl -- expr << expr
  | Sar -- expr >> expr
  | Shr -- expr >>> expr
  | BitAnd -- expr & expr
  | BitXor -- expr ^ expr
  | BitOr -- expr | expr
  | LessThan -- expr < expr
  | GreaterThan -- expr > expr
  | LessEqual -- expr <= expr
  | GreaterEqual -- expr >= expr
  | Equal -- expr == expr
  | NotEqual -- expr != expr
  | And -- expr && expr
  | Or -- expr || expr
  | Assign -- expr = expr
  | AssignBitOr -- expr |= expr
  | AssignBitXor -- expr ^= expr
  | AssignBitAnd -- expr &= expr
  | AssignShl -- expr <<= expr
  | AssignSar -- expr >>= expr
  | AssignShr -- expr >>>= expr
  | AssignAdd -- expr += expr
  | AssignSub -- expr -= expr
  | AssignMul -- expr *= expr
  | AssignDiv -- expr /= expr
  | AssignMod -- expr %= expr
  deriving stock (Show, Read, Eq, Ord, Generic)

showBinaryOp :: BinaryOp -> Expression -> Expression -> Text
showBinaryOp Exp e1 e2 = showExpression e1 <> " ** " <> showExpression e2
showBinaryOp Mul e1 e2 = showExpression e1 <> " * " <> showExpression e2
showBinaryOp Div e1 e2 = showExpression e1 <> " / " <> showExpression e2
showBinaryOp Mod e1 e2 = showExpression e1 <> " % " <> showExpression e2
showBinaryOp Add e1 e2 = showExpression e1 <> " + " <> showExpression e2
showBinaryOp Sub e1 e2 = showExpression e1 <> " - " <> showExpression e2
showBinaryOp Shl e1 e2 = showExpression e1 <> " << " <> showExpression e2
showBinaryOp Sar e1 e2 = showExpression e1 <> " >> " <> showExpression e2
showBinaryOp Shr e1 e2 = showExpression e1 <> " >>> " <> showExpression e2
showBinaryOp BitAnd e1 e2 = showExpression e1 <> " & " <> showExpression e2
showBinaryOp BitXor e1 e2 = showExpression e1 <> " ^ " <> showExpression e2
showBinaryOp BitOr e1 e2 = showExpression e1 <> " | " <> showExpression e2
showBinaryOp LessThan e1 e2 = showExpression e1 <> " < " <> showExpression e2
showBinaryOp GreaterThan e1 e2 = showExpression e1 <> " > " <> showExpression e2
showBinaryOp LessEqual e1 e2 = showExpression e1 <> " >= " <> showExpression e2
showBinaryOp GreaterEqual e1 e2 = showExpression e1 <> " <= " <> showExpression e2
showBinaryOp Equal e1 e2 = showExpression e1 <> " == " <> showExpression e2
showBinaryOp NotEqual e1 e2 = showExpression e1 <> " != " <> showExpression e2
showBinaryOp And e1 e2 = showExpression e1 <> " && " <> showExpression e2
showBinaryOp Or e1 e2 = showExpression e1 <> " || " <> showExpression e2
showBinaryOp Assign e1 e2 = showExpression e1 <> " = " <> showExpression e2
showBinaryOp AssignBitOr e1 e2 = showExpression e1 <> " |= " <> showExpression e2
showBinaryOp AssignBitXor e1 e2 = showExpression e1 <> " ^= " <> showExpression e2
showBinaryOp AssignBitAnd e1 e2 = showExpression e1 <> " &= " <> showExpression e2
showBinaryOp AssignShl e1 e2 = showExpression e1 <> " <<= " <> showExpression e2
showBinaryOp AssignSar e1 e2 = showExpression e1 <> " >>= " <> showExpression e2
showBinaryOp AssignShr e1 e2 = showExpression e1 <> " >>>= " <> showExpression e2
showBinaryOp AssignAdd e1 e2 = showExpression e1 <> " += " <> showExpression e2
showBinaryOp AssignSub e1 e2 = showExpression e1 <> " -= " <> showExpression e2
showBinaryOp AssignMul e1 e2 = showExpression e1 <> " *= " <> showExpression e2
showBinaryOp AssignDiv e1 e2 = showExpression e1 <> " /= " <> showExpression e2
showBinaryOp AssignMod e1 e2 = showExpression e1 <> " %= " <> showExpression e2

data ElementaryTypeName
  = Address
  | AddressPayable
  | Bool
  | String
  | Bytes
  | SignedInteger (Maybe Int)
  | UnsignedInteger (Maybe Int)
  | FixedBytes Int
  | Fixed
  | Ufixed
  deriving stock (Show, Read, Eq, Ord, Generic)

showElementayType :: ElementaryTypeName -> Text
showElementayType Address = "address"
showElementayType AddressPayable = "address payable"
showElementayType Bool = "bool"
showElementayType String = "string"
showElementayType Bytes = "bytes"
showElementayType (SignedInteger val) = T.pack $ "int" <> maybe "" show val
showElementayType (UnsignedInteger val) = T.pack $ "uint" <> maybe "" show val
showElementayType (FixedBytes val) = T.pack $ "bytes" <> show val
showElementayType Fixed = "fixed"
showElementayType Ufixed = "ufixed"

data Literal
  = StringLiteral Text
  | NumberLiteral Double (Maybe NumberUnit)
  | BooleanLiteral Bool
  | HexStringLiteral Text
  | UnicodeStringLiteral Text
  deriving stock (Show, Read, Eq, Ord, Generic)

showLiteral :: Literal -> Text
showLiteral (StringLiteral txt) = txt
showLiteral (NumberLiteral num unit) = T.pack $ show num <> maybe "" show unit
showLiteral (BooleanLiteral val) = T.pack $ show val
showLiteral (HexStringLiteral hex) = hex
showLiteral (UnicodeStringLiteral uni) = uni

data MappingKeyType
  = MappingElementaryType ElementaryTypeName
  | MappingIdentifier IdentifierPath
  deriving stock (Show, Read, Eq, Ord, Generic)

data MappingDefinition = MappingDefinition {mapping :: MappingKeyType, kind :: TypeName}
  deriving stock (Show, Read, Eq, Ord, Generic)

data TypeName
  = ElementaryType ElementaryTypeName
  | FunctionType FunctionDefinition
  | MappingType MappingDefinition
  | IdentifierType IdentifierPath
  | ArrayType TypeName [ArrayTypeStatus]
  deriving stock (Show, Read, Eq, Ord, Generic)

data ArrayTypeStatus = ArrayTypeEmpty | ArrayTypeExpression Expression
  deriving stock (Show, Read, Eq, Ord, Generic)

showTypeName :: TypeName -> Text
showTypeName (ElementaryType e) = showElementayType e
showTypeName (FunctionType _) = "func"
showTypeName (MappingType _) = "mapping"
showTypeName (IdentifierType i) = showIdentifierPath i
showTypeName (ArrayType name []) = showTypeName name
showTypeName (ArrayType name xs) = showTypeName name <> "[" <> T.concat (map showArrayType xs) <> "]"

showArrayType :: ArrayTypeStatus -> Text
showArrayType ArrayTypeEmpty = "[]"
showArrayType (ArrayTypeExpression expr) = showExpression expr
