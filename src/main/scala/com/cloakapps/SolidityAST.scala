package com.cloakapps

object SolidityAST 
{
	sealed trait ContractDefinition
	case class ContractDef(id:String,inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
	case class LibraryDef(id:String,inheritSpecs:List[InheritanceSpecifier], parts:List[ContractPart]) extends ContractDefinition
	// the above is like in ML
	// datatype contractDefinition = 
	//     contractDef of string * inheritanceSpecifier list * contractPart list  
	//   | libraryDef of string * inheritanceSpecifier list * contractPart list  

	case class InheritanceSpecifier(id:String,exps:List[Expression]) // have no idea what is this yet. can give an example?

	sealed trait ContractPart 
	case class StateVariableDeclaration(typeName:String,accessMod:Option[AccessModifier],id:String) extends ContractPart
	case class StructDefinition(id:String, varDecls:List[VariableDeclaration]) extends ContractPart
	case class ModifierDefinition(id:String, paras:List[Parameter],block:Block) extends ContractPart // To be continued
	case class FunctionDefinition(id:String, paras:List[Parameter],funcMod:FunctionModifier, retParas:List[Parameter], block:Block) extends ContractPart
	case class EnumDefinition(vals:List[EnumValue]) extends ContractPart

	type EnumValue = String

	sealed trait AccessModifier
	case object PublicAM extends AccessModifier
	case object PrivateAM extends AccessModifier
	case object InheritableAM extends AccessModifier

	sealed trait Expression // To be continued

	sealed trait VariableDeclaration

	sealed trait Block 

	sealed trait FunctionModifier  // Not sure whether can be merged with AccessModifier? TODO
	case class IdentifierFM(id:String) extends FunctionModifier
	case object ConstantFM extends FunctionModifier
	case object ExternalFM extends FunctionModifier
	case object PublicFM extends FunctionModifier
	case object InheritableFM extends FunctionModifier
	case object PrivateFM extends FunctionModifier


	sealed trait Parameter
}