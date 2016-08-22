package com.cloakapps

object SolidityAST
{
  // (ImportDirective | ContractDefinition)*
  //  sealed trait SourceUnit
  type SourceUnit = List[Either[ImportDirective,ContractDefinition]]

  // 'import' StringLiteral ('as' Identifier)? ';'
  // | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
  // | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'
  sealed trait ImportDirective // extends SourceUnit
  case class SimpleImport(module: StringLiteral, as: Option[Identifier]) extends ImportDirective
  case class FromImport(wildcardOrModule: Option[StringLiteral], as: Option[Identifier], from: StringLiteral) extends ImportDirective
  case class MultipleImport(modules: List[(Identifier, Option[Identifier])], from: StringLiteral) extends ImportDirective

  // ( 'contract' | 'library' ) Identifier
  // ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
  // '{' ContractPart* '}'
  sealed trait ContractDefinition // extends SourceUnit
  case class ContractDef(id: Identifier, inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
  case class LibraryDef(id: Identifier, inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
	// the above is like in ML
	// datatype contractDefinition = 
	//     contractDef of string * inheritanceSpecifier list * contractPart list  
	//   | libraryDef of string * inheritanceSpecifier list * contractPart list  

  // Identifier ( '(' Expression ( ',' Expression )* ')' )?
  case class InheritanceSpecifier(id:Identifier,exps:List[Expression]) // have no idea what is this yet. can give an example?

  // StateVariableDeclaration | UsingForDeclaration
  // | StructDefinition | ModifierDefinition | FunctionDefinition | EventDefinition | EnumDefinition
  sealed trait ContractPart
  // TypeName ( 'public' | 'internal' | 'private' )? Identifier ('=' Expression)? ';'
  case class StateVariableDeclaration(typeName:TypeName,accessMod:Option[AccessModifier],id:Identifier, exp: Expression) extends ContractPart
  // 'using' Identifier 'for' ('*' | TypeName) ';'
  case class UsingForDeclaration(id: Identifier, wildcardOrName: Option[TypeName]) extends ContractPart
  // 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
  case class StructDefinition(id:Identifier, varDecls:List[VariableDeclaration]) extends ContractPart
  // 'modifier' Identifier ParameterList? Block
  case class ModifierDefinition(id:Identifier, paras:List[Parameter],block:Block) extends ContractPart // To be continued
  // 'function' Identifier? ParameterList
  // ( FunctionCall | Identifier | 'constant' | 'external' | 'public' | 'internal' | 'private' )*
  // ( 'returns' ParameterList )? Block
  case class FunctionDefinition(id:Option[Identifier], paras:List[Parameter],funcMod:List[FunctionModifier], retParas:List[Parameter], block:Block) extends ContractPart

  // 'event' Identifier IndexedParameterList 'anonymous'? ';'
  case class EventDefinition(id: Identifier, params: List[Parameter], anonymous: Boolean) extends ContractPart

  // 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
  case class EnumDefinition(id: Identifier, vals:List[EnumValue]) extends ContractPart
  type EnumValue = Identifier

  sealed trait AccessModifier
  case object PublicAM extends AccessModifier
  case object PrivateAM extends AccessModifier
  case object InheritableAM extends AccessModifier

  sealed trait Expression extends ExpressionStatement

  sealed trait VariableDeclaration

	sealed trait FunctionModifier  // Not sure whether can be merged with AccessModifier? TODO
	case class FunctionCallFM(call: FunctionCall) extends FunctionModifier
	case class IdentifierFM(id:Identifier) extends FunctionModifier
	case object ConstantFM extends FunctionModifier
	case object ExternalFM extends FunctionModifier
	case object PublicFM extends FunctionModifier
	case object InheritableFM extends FunctionModifier
	case object PrivateFM extends FunctionModifier

	sealed trait Parameter
  // IndexedParameterList = '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'
  // ParameterList =        '(' ( TypeName            Identifier? (',' TypeName            Identifier?)* )? ')'
  case class IndexedParam(typeName: TypeName, id: Identifier, indexed: Boolean) extends Parameter
  // Note: The above definition of IndexParamList and ParameterList look redundant.

  // ElementaryTypeName | Identifier StorageLocation? | Mapping | ArrayTypeName
  sealed trait TypeName
  // 'address' | 'bool' | 'string' | 'var' | Int | Uint | Byte | Fixed | Ufixed
  sealed trait ElementaryTypeName extends TypeName
  // 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
  case class Mapping(elemType: ElementaryTypeName, typeName: TypeName) extends TypeName
  // TypeName StorageLocation? '[' Expression? ']'
  case class ArrayTypeName(typeName: TypeName, loc: Option[StorageLocation], exps: List[Expression]) extends TypeName
  case class StorageLocationTypeName(id: Identifier, loc: Option[StorageLocation]) extends TypeName

  // StorageLocation = 'memory' | 'storage'
  sealed trait StorageLocation
  case object Memory extends StorageLocation
  case object Storage extends StorageLocation

  // elementary types
  case object AddressType extends ElementaryTypeName
  case object BoolType extends ElementaryTypeName
  case object StringType extends ElementaryTypeName
  case object VarType extends ElementaryTypeName
  // Int type contains many sub types...
  case class IntType(typeName: String) extends ElementaryTypeName
  // Uint type contains many sub types...
  case class UintType(typeName: String) extends ElementaryTypeName
  // Byte type contains many sub types...
  case class ByteType(typeName: String) extends ElementaryTypeName
  // Fixed type contains many sub types...
  case class FixedType(typeName: String) extends ElementaryTypeName
  // Ufixed type contains many sub types...
  case class UfixedType(typeName: String) extends ElementaryTypeName

  // statements
  sealed trait Statement
  // '{' Statement* '}'
  case class Block(statements: List[Statement]) extends Statement
  // 'if' '(' Expression ')' Statement ( 'else' Statement )?
  case class IfStatement(cond: Expression, ifClause: Statement, elseClause: Option[Statement]) extends Statement
  // 'while' '(' Expression ')' Statement
  case class WhileStatement(cond: Expression, body: Statement) extends Statement

  // VariableDefinition | ExpressionStatement
  sealed trait SimpleStatement extends Statement
  // VariableDeclaration ( '=' Expression )?
  case class VariableDefinition(decl: VariableDeclaration, exp: Option[Expression]) extends SimpleStatement

  // Expression | VariableDefinition
  sealed trait ExpressionStatement extends Statement

  //  'for' '(' (SimpleStatement)? ';' (Expression)? ';' (ExpressionStatement)? ')' Statement
  case class ForStatement(init: Option[SimpleStatement], cond: Option[Expression], step: Option[ExpressionStatement], body: Statement) extends Statement

  // '_'
  case object PlaceHolderStatement extends Statement
  case object ContinueStatement extends Statement
  case object BreakStatement extends Statement
  // 'return' Expression?
  case class ReturnStatement(exp: Option[Expression]) extends Statement
  case object ThrowStatement extends Statement

  // expressions

  // '(' Expression ')'
  case class BracedExpression(exp: Expression) extends Expression
  // Identifier '(' Expression? ( ',' Expression )* ')'
  case class FunctionCall(name: Identifier, args: List[Expression]) extends Expression
  // 'new' Identifier
  case class NewExpression(id: Identifier) extends Expression
  // 'delete' Expression
  case class DeleteExpression(exp: Expression) extends Expression
  // Expression '.' Identifier
  case class MemberExpression(obj: Expression, member: Identifier) extends Expression
  // Expression '[' Expression? ']'
  case class IndexAccess(array: Expression, index: Option[Expression]) extends Expression
  // Expression? (',' Expression)
  case class Comma(first: Option[Expression], second: Option[Expression]) extends Expression

  sealed trait UnaryOperation extends Expression
  case class IncrementPostfix(exp: Expression) extends UnaryOperation
  case class DecrementPostfix(exp: Expression) extends UnaryOperation
  case class Negate(exp: Expression) extends UnaryOperation
  case class BitwiseNegate(exp: Expression) extends UnaryOperation
  case class IncrementPrefix(exp: Expression) extends UnaryOperation
  case class DecrementPrefix(exp: Expression) extends UnaryOperation
  case class UnaryPlus(exp: Expression) extends UnaryOperation
  case class UnaryMinus(exp: Expression) extends UnaryOperation
  case class UnaryShiftLeft(exp: Expression) extends UnaryOperation
  case class UnaryShiftRight(exp: Expression) extends UnaryOperation
  // TODO: didn't find detailed explanations of the above << and >> unary operators.
  // TODO: There's another unary >>> operator too, but didn't find any docs about it.

  sealed trait BinaryOperation extends Expression
  // **, *, /, %, +, -, &, |, ^, <, >, <=, >=, ==, !=, &&, ||
  case class Power(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Multiply(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Divide(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Remainder(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Plus(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Minus(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseAnd(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseXor(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseOr(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class LessThan(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class GreaterThan(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class LessOrEqual(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class GreaterOrEqual(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Equal(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class NotEqual(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class And(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class Or(lhs: Expression, rhs: Expression) extends BinaryOperation
  // assignment operators: =, |=, ^=, &=, <<=, >>=, +=, -=, *=, /=, %=
  case class Assign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseOrAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseXorAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class BitwiseAndAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class LeftShiftAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class RightShiftAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class PlusAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class MinusAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class MultiplyAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class DivideAssign(lhs: Expression, rhs: Expression) extends BinaryOperation
  case class RemainderAssign(lhs: Expression, rhs: Expression) extends BinaryOperation

  sealed trait TernaryExpression extends Expression
  // Expression '?' Expression ':' Expression
  case class IfThenElse(cond: Expression, trueClause: Expression, falseClause: Expression) extends TernaryExpression

  sealed trait PrimaryExpression extends Expression
  //  [a-zA-Z_] [a-zA-Z_0-9]*
  case class Identifier(id: String) extends PrimaryExpression
  //  'true' | 'false'
  sealed trait BooleanLiteral extends PrimaryExpression
  case object True extends BooleanLiteral
  case object False extends BooleanLiteral
  //  '0x'? [0-9]+ (' ' NumberUnit)?
  case class NumberLiteral(value: String, unit: Option[NumberUnit]) extends PrimaryExpression
  //  '"' ([^"\r\n\\] | '\\' .)* '"'
  case class StringLiteral(value: String) extends PrimaryExpression

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
}