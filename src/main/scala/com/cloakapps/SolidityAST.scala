package com.cloakapps

object SolidityAST
{
  // SourceUnit = (ImportDirective | ContractDefinition)*
  type SourceUnit = List[Either[ImportDirective,ContractDefinition]]

  // ImportDirective =  'import' StringLiteral ('as' Identifier)? ';'
  // | 'import' ('*' | StringLiteral) ('as' Identifier)? 'from' StringLiteral ';'
  // | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'
  sealed trait ImportDirective // extends SourceUnit
  case class SimpleImport(module: StringLiteral, as: Option[Identifier]) extends ImportDirective
  case class FromImport(wildcardOrModule: Option[StringLiteral], as: Option[Identifier], from: StringLiteral) extends ImportDirective
  case class MultipleImport(modules: List[(Identifier, Option[Identifier])], from: StringLiteral) extends ImportDirective

  // ContractDefinition = ( 'contract' | 'library' ) Identifier
  // ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
  // '{' ContractPart* '}'
  sealed trait ContractDefinition // extends SourceUnit
  case class ContractDef(id: Identifier, inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
  case class LibraryDef(id: Identifier, inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
	// the above is like in ML
	// datatype contractDefinition = 
	//     contractDef of string * inheritanceSpecifier list * contractPart list  
	//   | libraryDef of string * inheritanceSpecifier list * contractPart list  

  // InheritanceSpecifier = Identifier ( '(' Expression ( ',' Expression )* ')' )?
  case class InheritanceSpecifier(id:Identifier,exps:List[Expression]) // have no idea what is this yet. can give an example?
  // The inheritance specifier is just like a constructor call. example:
  //    contract PriceFeed is owned, mortal, named("GoldFeed") { ... }
  // where owned, mortal and named are all contract themselves, and the "named" contract has a function defined as below:
  //    function named(string32 name) { ... }

  // -- original --
  // ContractPart = StateVariableDeclaration | UsingForDeclaration
  // | StructDefinition | ModifierDefinition | FunctionDefinition | EventDefinition | EnumDefinition
  // -- rewrite to include function declaration in contract interfaces.
  // ContractPart = StateVariableDeclaration | UsingForDeclaration
  // | StructDefinition | ModifierDefinition | FunctionDefinition | EventDefinition | EnumDefinition | FunctionDeclaration 
  sealed trait ContractPart

  // Modifiers can be applied to both variables and functions
  //
  // http://solidity.readthedocs.io/en/latest/miscellaneous.html#modifiers
  // 
  // Example: function register() payable costs(price) { ... }
  // where costs(price) is a custom modifier defined somewhere else
  //
  // -- write into BNF --
  // VisibilitySpec = 'external' | 'internal' | 'public' | 'private'
  // Modifier = 'constant' | 'anonymous' | 'payable' | FunctionCall | Identifier
  sealed trait VisibilitySpec
  case object ExternalSpec extends VisibilitySpec
  case object InternalSpec extends VisibilitySpec
  case object PublicSpec extends VisibilitySpec
  case object PrivateSpec extends VisibilitySpec
  sealed trait Modifier
  case object ConstantModifier extends Modifier
  case object AnonymousModifier extends Modifier
  case object PayableModifier extends Modifier
  case object IndexedModifier extends Modifier
  case class FunctionCallModifier(call: FunctionCallExpr) extends Modifier
  case class IdentifierModifier(id:Identifier) extends Modifier

  // -- original --
  // StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' )? Identifier ('=' Expression)? ';'
  // -- BNF bug: modifiers not included. rewrite --
  // StateVariableDeclaration = TypeName ( VisibilitySpec | Modifier )* Identifier ('=' Expression)? ';'
  // Note: state variables are always in storage.
  case class StateVariableDeclaration(typeName:TypeName,mod:List[Either[VisibilitySpec,Modifier]],id:Identifier, exp: Option[Expression]) extends ContractPart
  // UsingForDeclaration = 'using' Identifier 'for' ('*' | TypeName) ';'
  // Note: using A for B is like adding method extensions from library A to type B.
  case class UsingForDeclaration(id: Identifier, wildcardOrName: Option[TypeName]) extends ContractPart
  // StructDefinition = 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
  case class StructDefinition(id:Identifier, varDecls:List[VariableDeclaration]) extends ContractPart
  // Note: Looks like the above is just:
  // StructDefinition = 'struct' Identifier '{' ( VariableDeclaration ';' )* '}'

  // ModifierDefinition = 'modifier' Identifier ParameterList? Block
  case class ModifierDefinition(id:Identifier, paras:ParameterList, block:Block) extends ContractPart 


  // -- original --
  // FunctionDefinition = 'function' Identifier? ParameterList
  // ( FunctionCall | Identifier | 'constant' | 'external' | 'public' | 'internal' | 'private' )*
  // ( 'returns' ParameterList )? Block
  // -- rewrite --
  // FunctionDefinition = 'function' Identifier? ParameterList ( VisibilitySpec | Modifier )* ( 'returns' ParameterList )? Block
  case class FunctionDefinition(id:Option[Identifier], paras:ParameterList, mod:List[Either[VisibilitySpec,Modifier]], retParas:ParameterList, block:Block) extends ContractPart
  // -- BNF bug: There's also function declaration without body
  // FunctionDeclaration = 'function' Identifier? ParameterList ( VisibilitySpec | Modifier )* ( 'returns' ParameterList )? ';'
  case class FunctionDeclaration(id:Option[Identifier], paras:ParameterList, mod:List[Either[VisibilitySpec,Modifier]], retParas:ParameterList) extends ContractPart


  // EventDefinition = 'event' Identifier IndexedParameterList 'anonymous'? ';'
  case class EventDefinition(id: Identifier, params: ParameterList, mod: Option[Modifier]) extends ContractPart

  // EnumDefinition = 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
  case class EnumDefinition(id: Identifier, vals:List[EnumValue]) extends ContractPart
  type EnumValue = Identifier

  // -- original --
  // VariableDeclaration = TypeName Identifier
  // -- rewrite since variable declarations may come with location specifiers
  // VariableDeclaration = LocTypeName Identifier
  // Note: variable declarations in struct does not come with location specifiers. 
  // But just keep it simple here.
  case class  VariableDeclaration(typeName:LocTypeName, id:Identifier)

  // -- original --
  // IndexedParameterList = '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'
  // ParameterList =        '(' ( TypeName            Identifier? (',' TypeName            Identifier?)* )? ')'
  // -- The above look redundant. Also rewrite to fix the location specifier bug in TypeName --
  // -- rewrite --
  // Parameter = LocTypeName 'indexed'? Identifier?
  // ParameterList = '(' (Parameter (',' Parameter)*)? ')'
  case class Parameter(typeName: LocTypeName, id: Option[Identifier], indexed: Boolean) 
  case class ParameterList(list: List[Parameter]) 

  // -- original --
  // TypeName = ElementaryTypeName | Identifier StorageLocation? | Mapping | ArrayTypeName
  // -- There's a bug above. Location specifiers should follow another type name, and cannot appear in mappings.
  // -- first rewrite --
  // TypeName = ElementaryTypeName | Mapping | ArrayTypeName | Identifier
  // LocTypeName = TypeName StorageLocation?
  // -- second rewrite: remove left recursion --
  // TypeName = TypeNameHead TypeNameTail?
  // TypeNameHead = ElementaryTypeName | Mapping | Identifier
  // TypeNameTail = TypeNameTailStart TypeNameTail?
  // TypeNameTailStart = ArrayTypeNameTail
  sealed trait TypeNameHead
  sealed trait TypeNameTailStart
  case class TypeNameTail(start: TypeNameTailStart, next: Option[TypeNameTail])
  case class TypeName(head: TypeNameHead, tail: Option[TypeNameTail])
  case class LocTypeName(name: TypeName, loc: Option[StorageLocation])

  // ElementaryTypeName = 'address' | 'bool' | 'string' | 'var' | Int | Uint | Byte | Fixed | Ufixed
  sealed trait ElementaryTypeName extends TypeNameHead // \kl: maybe it's better to introduce a tag
  case class ElementaryType(name: ElementaryTypeName) 

  // Mapping = 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
  case class Mapping(elemType: ElementaryType, typeName: TypeName) extends TypeNameHead

  case class CustomType(id: Identifier) extends TypeNameHead

  // -- original --
  // ArrayTypeName = TypeName StorageLocation? '[' Expression? ']'
  // -- There's a bug above. location specifier is after [] and should only appear in places other than inside mapping.
  // -- first rewrite --
  // ArrayTypeName = TypeName '[' Expression? ']'
  // -- second rewrite: remove left recursion --
  // ArrayTypeNameTail = '[' Expression? ']'
  case class ArrayTypeNameTail(exp: Option[Expression]) extends TypeNameTailStart

  // StorageLocation = 'memory' | 'storage'
  sealed trait StorageLocation
  case object Memory extends StorageLocation
  case object Storage extends StorageLocation

  // elementary types
  case object AddressType extends ElementaryTypeName
  case object BoolType extends ElementaryTypeName
  case object StringType extends ElementaryTypeName
  case object VarType extends ElementaryTypeName
  // Int types. e.g., int, int8, ..., int256
  case class IntType(typeName: String) extends ElementaryTypeName
  // Uint types. e.g., uint, uint8, ..., uint256
  case class UintType(typeName: String) extends ElementaryTypeName
  // Byte types. e.g., byte, bytes1, ..., bytes32
  case class ByteType(typeName: String) extends ElementaryTypeName
  // Fixed types. e.g., fixed, fixed0x8, ...
  case class FixedType(typeName: String) extends ElementaryTypeName
  // Ufixed type. e.g., ufixed, ufixed0x8, ...
  case class UfixedType(typeName: String) extends ElementaryTypeName

  //  Block = '{' Statement* '}'
  sealed trait Block extends Statement //\kl Block is not a statement
  case class BlockStatement(statements: List[Statement]) extends Block // if block were not a statement we have to redefine if/while etc.

  // statements
  // ---- original definition ---------
  // Statement = IfStatement | WhileStatement | ForStatement | Block | PlaceholderStatement |
  //             ( Continue | Break | Return | Throw | SimpleStatement | ExpressionStatement ) ';'
  // SimpleStatement = VariableDefinition | ExpressionStatement
  // ExpressionStatement = Expression | VariableDefinition // \kl VariableDefintion is also in SimpleStatement
  // ----- written definition -----------
  // \kl Re-writing as follows
  // Statement = IfStatement | WhileStatement | ForStatement | Block | PlaceholderStatement |
  //             ( Continue | Break | Return | Throw | VariableDefinition | Expression ) ';'
  // SimpleStatement = VariableDefinition | Expression
  // ExpressionStatement = VariableDefinition | Expression 

  sealed trait Statement
  // IfStatement = 'if' '(' Expression ')' Statement ( 'else' Statement )?
  case class IfStatement(cond: Expression, ifClause: Statement, elseClause: Option[Statement]) extends Statement
  // WhileStatement = 'while' '(' Expression ')' Statement
  case class WhileStatement(cond: Expression, body: Statement) extends Statement

  // VariableDefintion = VariableDeclaration ( '=' Expression )?
  case class VariableDefinition(decl: VariableDeclaration, expr: Option[Expression]) 

  // type for SimpleStatement. this is needed by the ForStatement
  type VarDefOrExpression = Either[VariableDefinition,Expression]
  // a case class for SimpleStatement, needed by Statement
  case class SimpleStatement(body: Either[VariableDefinition,Expression]) extends Statement

  case class ExpressionStatement(expr: Expression) extends Statement

  // ForStatement = 
  //  -- original -- 'for' '(' (SimpleStatement)? ';' (Expression)? ';' (ExpressionStatement)? ')' Statement
  //  -- written  -- 'for' '(' (SimpleStatement)? ';' (Expression)? ';' (Expression)? ')' Statement
  case class ForStatement(init: Option[VarDefOrExpression], cond: Option[Expression], step: Option[Expression], body: Statement) extends Statement

  // PlaceholderStatement = '_'
  case object PlaceHolderStatement extends Statement
  // Continue = 'continue'
  case object ContinueStatement extends Statement
  // Break = 'break'
  case object BreakStatement extends Statement
  // Return = 'return' Expression?
  case class ReturnStatement(exp: Option[Expression]) extends Statement
  // Throw = 'throw'
  case object ThrowStatement extends Statement

  // expressions
  /*
	Expression =
	  ( Expression ('++' | '--') | FunctionCall | IndexAccess | MemberAccess | '(' Expression ')' )
	  | ('!' | '~' | 'delete' | '++' | '--' | '+' | '-') Expression
	  | Expression '**' Expression
	  | Expression ('*' | '/' | '%') Expression
	  | Expression ('+' | '-') Expression
	  | Expression ('<<' | '>>' | '>>>')
	  | Expression '&' Expression
	  | Expression '^' Expression
	  | Expression '|' Expression
	  | Expression ('<' | '>' | '<=' | '>=') Expression
	  | Expression ('==' | '!=') Expression
	  | Expression '&&' Expression
	  | Expression '||' Expression
	  | Expression '?' Expression ':' Expression
	  | Expression ('=' | '|=' | '^=' | '&=' | '<<=' | '>>=' | '+=' | '-=' | '*=' | '/=' | '%=') Expression
	  | Expression? (',' Expression)
	  | PrimaryExpression
  */
  // Note: BNF bug: The NewExpression should be included in Expression
  //sealed trait Expression

  // left recursion occurs for:
  // IndexAccess, MethodCall, postfix unary operations, binary operations, comma operation
  // Rewrite expression as
  //   Expression = ExpressionHead ExpressionTail?
  // where ExpressionHead are original definitions that do not cause left recursion 
  //   ExpressionHead = FunctionCall | '(' Expression ')' | UnaryOperation
  //     | NewExpression | DeleteExpression | PrefixUnaryOperation | PrimaryExpression
  // and 
  //   ExpressionTailStart = IndexAccessTail | MemberAccessTail | MethodCallTail 
  //     | UnaryOperationTail | BinaryOperationTail | TernaryOptionTail | CommaTail 
  //   ExpressionTail = ExpressionTailStart ExpressionTail?
  sealed trait ExpressionHead
  sealed trait ExpressionTailStart
  case class ExpressionTail(start: ExpressionTailStart, next: Option[ExpressionTail])
  case class Expression(head: ExpressionHead, tail: Option[ExpressionTail])

  // '(' Expression ')'
  case class EnclosedExpression(exp: Expression) extends ExpressionHead
  // -- original --
  // FunctionCall = Identifier '(' Expression? ( ',' Expression )* ')'
  // -- looks wrong. rewrite --
  // FunctionCall = Identifier '(' (Expression ( ',' Expression )* )? ')'
  sealed trait FunctionCall extends ExpressionHead
  case class FunctionCallExpr(name: Identifier, args: List[Expression]) extends FunctionCall

  // 'new' Expression
  case class NewExpression(exp: Expression) extends ExpressionHead
  // 'delete' Expression
  case class DeleteExpression(exp: Expression) extends ExpressionHead

  // Note: BNF bug: FunctionCall is not enough to capture "recipient[1].address.value(10000).call()"
  // Note: the call() method can be in the form of address.call.value(amount).gas(amount)(parameter)
  // was: 
  // MethodCall = Expression '.' Identifier '(' (Expression ( ',' Expression )* )? ')'
  // now: 
  // MethodCall = Expression '.' Identifier ( '.' FunctionCallExpr )* '(' (Expression ( ',' Expression )* )? ')'
  //case class MethodCall(obj: Expression, name: Identifier, args: List[Expression]) extends Expression
  //case class MethodCallTail(name: Identifier, args: List[Expression]) extends ExpressionTailStart
  case class MethodCallTail(name: Identifier, attributes:List[FunctionCallExpr], args: List[Expression]) extends ExpressionTailStart

  
  // Expression '.' Identifier
  //case class MemberAccess(obj: Expression, member: Identifier) extends Expression
  case class MemberAccessTail(member: Identifier) extends ExpressionTailStart
  // Expression '[' Expression? ']'
  //case class IndexAccess(array: Expression, index: Option[Expression]) extends Expression
  case class IndexAccessTail(index: Option[Expression]) extends ExpressionTailStart

  // -- original -- Expression? (',' Expression)
  // The above looks suspicious. Might be:
  // -- rewritten -- Expression (',' Expression)?
  //case class Comma(first: Expression, second: Option[Expression]) extends Expression
  case class CommaTail(rest: Option[Expression]) extends ExpressionTailStart

  sealed trait UnaryOperation extends ExpressionHead
  sealed trait UnaryOperationTail extends ExpressionTailStart

  // Expression ++
  case object IncrementPostfixTail extends UnaryOperationTail
  // Expression --
  case object DecrementPostfixTail extends UnaryOperationTail

  // ~ Expression
  case class Negate(exp: Expression) extends UnaryOperation
  // ! Expression
  case class BitwiseNegate(exp: Expression) extends UnaryOperation
  // ++ Expression
  case class IncrementPrefix(exp: Expression) extends UnaryOperation
  // -- Expression
  case class DecrementPrefix(exp: Expression) extends UnaryOperation
  // + Expression
  case class UnaryPlus(exp: Expression) extends UnaryOperation
  // - Expression
  case class UnaryMinus(exp: Expression) extends UnaryOperation
  // Expression << 
  case object UnaryShiftLeftTail extends UnaryOperationTail
  // Expression >> 
  case object UnaryShiftRightTail extends UnaryOperationTail
  // TODO: didn't find detailed explanations of the above << and >> unary operators.
  // TODO: There's another unary >>> operator too, but didn't find any docs about it.

  sealed trait BinaryOperation extends ExpressionTailStart
  // **, *, /, %, +, -, &, |, ^, <, >, <=, >=, ==, !=, &&, ||
  // Expression ** Expression
  case class Power(rhs: Expression) extends BinaryOperation
  // Expression * Expression
  case class Multiply(rhs: Expression) extends BinaryOperation
  // Expression / Expression
  case class DivideBy(rhs: Expression) extends BinaryOperation
  // Expression % Expression
  case class Remainder(rhs: Expression) extends BinaryOperation
  // Expression + Expression
  case class Add(rhs: Expression) extends BinaryOperation
  // Expression - Expression
  case class Subtract(rhs: Expression) extends BinaryOperation
  // Expression & Expression
  case class BitwiseAnd(rhs: Expression) extends BinaryOperation
  // Expression | Expression
  case class BitwiseXor(rhs: Expression) extends BinaryOperation
  // Expression ^ Expression
  case class BitwiseOr(rhs: Expression) extends BinaryOperation
  // Expression < Expression
  case class LessThan(rhs: Expression) extends BinaryOperation
  // Expression > Expression
  case class GreaterThan(rhs: Expression) extends BinaryOperation
  // Expression <= Expression
  case class LessOrEqual(rhs: Expression) extends BinaryOperation
  // Expression >= Expression
  case class GreaterOrEqual(rhs: Expression) extends BinaryOperation
  // Expression == Expression
  case class EqualTo(rhs: Expression) extends BinaryOperation
  // Expression != Expression
  case class NotEqual(rhs: Expression) extends BinaryOperation
  // Expression && Expression
  case class And(rhs: Expression) extends BinaryOperation
  // Expression || Expression
  case class Or(rhs: Expression) extends BinaryOperation
  // assignment operators: =, |=, ^=, &=, <<=, >>=, +=, -=, *=, /=, %=
  // Expression = Expression
  case class Assign(rhs: Expression) extends BinaryOperation
  // Expression |= Expression
  case class BitwiseOrAssign(rhs: Expression) extends BinaryOperation
  // Expression ^= Expression
  case class BitwiseXorAssign(rhs: Expression) extends BinaryOperation
  // Expression &= Expression
  case class BitwiseAndAssign(rhs: Expression) extends BinaryOperation
  // Expression <<= Expression
  case class LeftShiftAssign(rhs: Expression) extends BinaryOperation
  // Expression >>= Expression
  case class RightShiftAssign(rhs: Expression) extends BinaryOperation
  // Expression += Expression
  case class PlusAssign(rhs: Expression) extends BinaryOperation
  // Expression -= Expression
  case class MinusAssign(rhs: Expression) extends BinaryOperation
  // Expression *= Expression
  case class MultiplyAssign(rhs: Expression) extends BinaryOperation
  // Expression /= Expression
  case class DivideAssign(rhs: Expression) extends BinaryOperation
  // Expression %= Expression
  case class RemainderAssign(rhs: Expression) extends BinaryOperation

  sealed trait TernaryExpression extends ExpressionTailStart
  // Expression '?' Expression ':' Expression
  case class IfThenElse(trueClause: Expression, falseClause: Expression) extends TernaryExpression

  // PrimaryExpression = Identifier | BooleanLiteral | NumberLiteral | StringLiteral

  sealed trait PrimaryExpression extends ExpressionHead
  //  Identifier = [a-zA-Z_] [a-zA-Z_0-9]* // this constraint will be enforced by the parser
  // \kl Identifier is a type, it is not recommended to keep it as case class
  type Identifier = String
  case class IdentifierExpr(id: Identifier) extends PrimaryExpression
  //  BooleanLiteral = 'true' | 'false'
  sealed trait BooleanLiteral extends PrimaryExpression
  case object True extends BooleanLiteral
  case object False extends BooleanLiteral
  //  NumberLiteral = '0x'? [0-9]+ (' ' NumberUnit)?
  case class NumberLiteral(value: String, unit: Option[NumberUnit]) extends PrimaryExpression

  // ref: http://solidity.readthedocs.io/en/latest/types.html?highlight=string%20literal#string-literals
  // String literals start and end with either a single or a double quote, 
  // and escape sequences are of the form: \\n, \\xNN and \\uNNNN (single backslash)
  // There's a hex literal type like string in the form of hex"00ff"
  // TODO: The following definition has to be re-written.
  //  StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
  type StringLiteral = String
  case class StringLiteralExpr(value: String) extends PrimaryExpression

  sealed trait NumberUnit
  sealed trait MoneyUnit extends NumberUnit
  case object Wei extends MoneyUnit
  case object Szabo extends MoneyUnit
  case object Finney extends MoneyUnit
  case object Ether extends MoneyUnit

  sealed trait TimeUnit extends NumberUnit
  case object Seconds extends TimeUnit
  case object Minutes extends TimeUnit
  case object Hours extends TimeUnit
  case object Days extends TimeUnit
  case object Weeks extends TimeUnit
  case object Years extends TimeUnit
  /*
  sealed trait GlobalFunctionCall extends FunctionCall
  // block.blockhash(uint blockNumber) returns (bytes32)
  case class BlockHash(blockNumber: Expression) extends GlobalFunctionCall
  // sha3(...) returns (bytes32)
  case class Sha3(params:Expression) extends GlobalFunctionCall
  // sha256(...) returns (bytes32)
  case class Sha256(params:Expression) extends GlobalFunctionCall
  // ripemd160(...) returns (bytes32)
  case class Ripemd160(params:Expression) extends GlobalFunctionCall
  // ecrecover(byte32 hash, uin8 v, byte32 r, byte32 s) returns (address)
  case class ECRecover(hash:Expression, v: Expression, r: Expression, s: Expression) extends GlobalFunctionCall
  // addmod(uint x, uint y, uint k) returns (uint)
  case class AddMod(x: Expression, y: Expression, k: Expression) extends GlobalFunctionCall
  // mulmod(uint x, uint y, uint k) returns (uint)
  case class MulMod(x: Expression, y: Expression, k: Expression) extends GlobalFunctionCall
  // selfdestruct(address recipient)
  case class SelfDestruct(recipient: Expression) extends GlobalFunctionCall

  // TODO: how to model <address>.balance : uint256
  // TODO: how to model <address>.send(uint256 amount) returns (bool) : uint256
  
  sealed trait GlobalVariable extends Identifier
  // now: uint -- alias to block.timestamp
  case object Now extends GlobalVariable
  // this
  case object This extends GlobalVariable
  // super
  case object Super extends GlobalVariable

  sealed trait GlobalObjMember extends MemberExpression
  // block.coinbase: address
  case object BlockCoinBase extends GlobalObjMember
  // block.difficulty: uint
  case object BlockDifficulty extends GlobalObjMember
  // block.gaslimit: uint
  case object BlockGasLimit extends GlobalObjMember
  // block.number: uint
  case object BlockNumber extends GlobalObjMember
  // block.timestamp: uint
  case object BlockTimestamp extends GlobalObjMember

  // msg.data: bytes
  case object MsgData extends GlobalObjMember
  // msg.gas: uint
  case object MsgGas extends GlobalObjMember
  // msg.sender: address
  case object MsgSender extends GlobalObjMember
  // msg.value: uint
  case object MsgValue extends GlobalObjMember

  // tx.gasprice: Uint
  case object TxGasPrice extends GlobalObjMember
  // tx.origin: address
  case object TxOrigin extends GlobalObjMember
  */
}