package com.cloakapps

import scalaz._
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.NonBacktracking._
import com.cloakapps.SolParserPrimitive._
import com.cloakapps.SolidityAST._

/**
  * Created by Qiming Li on 8/20/2016.
  */
object SolParser {
  /*
   * To test a specific parser in console:
   *
   * import com.cloakapps._;  import SolParser._; import SolParserPrimitive._
   * 
   * parse(stringLiteral)("'abc'")
   */

  def parse[A](pa:Parser[A])(x:String):Result[Option[(A,List[Token])]] = {
    def m:Parser[A] = for {
      a <- pa
      _ <- eof
    } yield a
    run(m)(x.toList)
  }

  def parseSol(x:String):Result[Option[(SourceUnit,List[Token])]] = parse(sourceUnit)(x)

  def getLoc:Parser[Int] = for
  {
    tokens <- getState
  } yield tokens.length

  def sourceUnit:Parser[SourceUnit] = for {
    _     <- whiteSpaces
    units <- many(either1(importDirective)(contractDefinition))
  } yield units.map(toEither(_))

  def importDirective:Parser[ImportDirective] = +++(attempt(simpleImport))(+++(attempt(fromImport))(multipleImport))


  // +++(p1)(p2) execute p1 if it can proceed, otherwise p2
  // note that the parser combinator are by-default not back-tracking.
  // since simple , from and multiple imports are all starting with the keyword "import", we need to insert attempt()
  // to make the alternatives backtrackable.


  def simpleImport:Parser[ImportDirective] = for
  {
    _        <- string("import")
    _        <- whiteSpaces
    strLit   <- stringLiteral
    _        <- whiteSpaces
    maybeId  <- optional(asIdentifier)
    asId = maybeId match
    {
      case -\/(id) => Some(id) // left
      case \/-(_)  => None     // right
    }
    _        <- whiteSpaces
    _        <- char(';')
  } yield SimpleImport(strLit, asId)


  def fromImport:Parser[ImportDirective] = for
  {
    _               <- string("import")
    _               <- whiteSpaces
    asterixOrStrLit <- either1(char('*'))(stringLiteral)
    wildcardOrModule = asterixOrStrLit match
    {
      case -\/(_)   => None
      case \/-(mod) => Some(mod)
    }
    _        <- whiteSpaces
    maybeId  <- optional(asIdentifier)
    asId = maybeId match
    {
      case -\/(id) => Some(id) // left
      case \/-(_)  => None     // right
    }
    _        <- whiteSpaces
    _        <- string("from")
    _        <- whiteSpaces
    from     <- stringLiteral
    _        <- char(';')
  } yield FromImport(wildcardOrModule,asId,from)


  def multipleImport:Parser[ImportDirective] =
  {
    def idAsIdOpt:Parser[(Identifier,Option[Identifier])] = for
    {
      id <- identifier
      _  <- whiteSpaces
      maybeId  <- optional(asIdentifier)
      asId = maybeId match
      {
        case -\/(id) => Some(id) // left
        case \/-(_)  => None     // right
      }
    } yield (id,asId)
    for
    {
      _          <- string("import")
      _          <- whiteSpaces
      _		       <- char('{')
      _          <- whiteSpaces
      modules    <- interleave(idAsIdOpt)(seq(whiteSpaces,char(',')))
      _          <- whiteSpaces
      _          <- string("from")
      _          <- whiteSpaces
      from       <- stringLiteral
      _          <- char(';')
    } yield MultipleImport(modules, from)
  }

  def asIdentifier:Parser[Identifier] = for
  {
    _  <- string("as")
    _  <- whiteSpaces
    id <- identifier
  } yield (id)

  // expressions

  def identifier:Parser[Identifier] = for {
    c  <- sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z'))
    cs <- many( sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z') || x.isDigit ) )
  } yield (c::cs).mkString

  def identifierExpr:Parser[ExpressionHead] = for {
    s <- identifier
  } yield IdentifierExpr(s)

  def hexLiteral:Parser[StringLiteral] = for {
    _ <- string("hex")
    open <- sat(x => x == '\'' || x == '"')
    h <- hex
    _ <- char(open)
  } yield "0x" + h

  def quotedLiteral:Parser[StringLiteral] = {
    def escaped:Parser[List[Char]] = for {
      s <- char('\\')
      c <- item
    } yield List(s,c)

    def normal(c:Char):Parser[List[Char]] = for {
      c <- sat(x => x != '\r' && x != '\n' && x != c)
    } yield List(c)

    for {
        open <- sat(x => x == '\'' || x == '"')
        cs <- many(anyAttempt(List(escaped, normal(open))))
        _ <- char(open)
    } yield cs.flatten.mkString
  }

  def stringLiteral:Parser[StringLiteral] = anyAttempt(List(hexLiteral, quotedLiteral))

  def stringLiteralExpr:Parser[ExpressionHead] = for {
    s <- stringLiteral
  } yield StringLiteralExpr(s)

  // booleans
  def trueLiteral:Parser[ExpressionHead] = for {
    _ <- string("true")
  } yield True
  def falseLiteral:Parser[ExpressionHead] = for {
    _ <- string("false")
  } yield False
  def booleanLiteral:Parser[ExpressionHead] = anyAttempt(List(trueLiteral, falseLiteral))

  // money units
  def wei:Parser[NumberUnit] = for { _ <- string("wei") } yield Wei
  def szabo:Parser[NumberUnit] = for { _ <- string("szabo") } yield Szabo
  def finney:Parser[NumberUnit] = for { _ <- string("finney") } yield Finney
  def ether:Parser[NumberUnit] = for { _ <- string("ether") } yield Ether
  def moneyUnit:Parser[NumberUnit] = anyAttempt(List(wei, szabo, finney, ether))

  // time units
  def seconds:Parser[NumberUnit] = for { _ <- string("seconds") } yield Seconds
  def minutes:Parser[NumberUnit] = for { _ <- string("minutes") } yield Minutes
  def hours:Parser[NumberUnit] = for { _ <- string("hours") } yield Hours
  def days:Parser[NumberUnit] = for { _ <- string("days") } yield Days
  def weeks:Parser[NumberUnit] = for { _ <- string("weeks") } yield Weeks
  def years:Parser[NumberUnit] = for { _ <- string("years") } yield Years
  def timeUnit:Parser[NumberUnit] = anyAttempt(List(seconds, minutes, hours, days, weeks, years))

  def numberUnit:Parser[NumberUnit] = for {
    _ <- whiteSpace1
    u <- anyAttempt(List(moneyUnit, timeUnit))
  } yield u

  def numberLiteral:Parser[ExpressionHead] = for {
    prefix <- optional(string("0x"))
    cs <- many1(digit)
    maybeUnit <- optional(numberUnit)
  } yield NumberLiteral(toOption(prefix).getOrElse("") + cs.mkString, toOption(maybeUnit))

  def primaryExpression:Parser[ExpressionHead] = anyAttempt(List(identifierExpr, booleanLiteral, numberLiteral, stringLiteralExpr))

  def enclosedExpression:Parser[ExpressionHead] = for {
    _ <- sep("(")
    exp <- expression
    _ <- sep(")")
  } yield EnclosedExpression(exp)

  def functionCallArgs:Parser[List[Expression]] = for {
    _ <- whiteSpaces
    _ <- char('(')
    _ <- whiteSpaces
    args <- interleave(expression)(sep(","))
    _ <- whiteSpaces
    _ <- char(')')
  } yield args

  def functionCallExpr:Parser[FunctionCallExpr] = for {
  	name <- identifier
    args <- functionCallArgs
  } yield FunctionCallExpr(name, args)

  def functionCall:Parser[ExpressionHead] = for {
    f <- functionCallExpr
  } yield f

  def methodCallTail:Parser[ExpressionTail] = for {
    //obj <- expression
    _ <- spaceString(".")
    name <- identifier
    _ <- sep("(")
    args <- interleave(expression)(sep(","))
    _ <- sep(")")
  } yield MethodCallTail(name, args)

  def newExpression:Parser[ExpressionHead] = for {
    _ <- string("new")
    _ <- many1(whiteSpace)
    id <- identifier
  } yield NewExpression(id)

  def delExpression:Parser[ExpressionHead] = for {
    _ <- string("delete")
    _ <- many1(whiteSpace)
    exp <- expression
  } yield DeleteExpression(exp)

  def memberAccessTail:Parser[ExpressionTail] = for {
    //exp <- expression
    _ <- spaceString(".")
    id <- identifier
  } yield MemberAccessTail(id)

  def indexAccessTail:Parser[ExpressionTail] = for {
    //exp <- expression
    _ <- sep("[")
    ind <- optional(expression)
    _ <- sep("]")
  } yield IndexAccessTail(toOption(ind))

  def commaTail:Parser[ExpressionTail] = for {
    //first <- expression
    _ <- sep(",")
    second <- optional(expression)
  } yield CommaTail(toOption(second))

  def postfixUnaryOp:Parser[String] = anyAttempt(List("++", "--", ">>", "<<").map(s => string(s)))
  def prefixUnaryOp:Parser[String] = anyAttempt(List("++", "--", "!", "~", "+", "-").map(s => string(s)))
  def binaryOp:Parser[String] = anyAttempt(List("**", 
  	"==", "!=", "<=", ">=", "<", ">", 
  	"*=", "/=", "%=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", 
  	"*", "/", "%", "+", "-", "&&", "||", "&", "|", "^", "=").map(s => string(s)))

  def postfixUnaryOperation:Parser[ExpressionTail] = for {
    //exp <- expression
    _ <- whiteSpaces
    op <- postfixUnaryOp
  } yield op match {
    case "++" => IncrementPostfixTail
    case "--" => DecrementPostfixTail
    case "<<" => UnaryShiftLeftTail
    case ">>" => UnaryShiftRightTail
  }

  def prefixUnaryOperation:Parser[ExpressionHead] = for {
  	op <- prefixUnaryOp
    _ <- whiteSpaces
  	exp <- expression
  } yield op match {
  	case "++" => IncrementPrefix(exp)
  	case "--" => DecrementPrefix(exp)
  	case "!" => Negate(exp)
  	case "~" => BitwiseNegate(exp)
  	case "+" => UnaryPlus(exp)
  	case "-" => UnaryMinus(exp)
  }

  def unaryOperation:Parser[ExpressionHead] = attempt(prefixUnaryOperation)
  def unaryOperationTail:Parser[ExpressionTail] = attempt(postfixUnaryOperation)

  def binaryOperationTail:Parser[ExpressionTail] = for {
  	//lhs <- expression
  	op <- binaryOp
  	rhs <- expression
  } yield op match {
  	case "**" => Power(rhs)
  	case "==" => EqualTo(rhs)
  	case "!=" => NotEqual(rhs)
  	case "<=" => LessOrEqual(rhs)
  	case ">=" => GreaterOrEqual(rhs)
  	case "<"  => LessThan(rhs)
  	case ">"  => GreaterThan(rhs)
  	case "*=" => MultiplyAssign(rhs)
  	case "/=" => DivideAssign(rhs)
  	case "%=" => RemainderAssign(rhs)
  	case "|=" => BitwiseOrAssign(rhs)
  	case "^=" => BitwiseXorAssign(rhs)
  	case "&=" => BitwiseAndAssign(rhs)
  	case "<<=" => LeftShiftAssign(rhs)
  	case ">>=" => RightShiftAssign(rhs)
  	case "+=" => PlusAssign(rhs)
  	case "-=" => MinusAssign(rhs)
  	case "*" => Multiply(rhs)
  	case "/" => DivideBy(rhs)
  	case "%" => Remainder(rhs)
  	case "+" => Add(rhs)
  	case "-" => Subtract(rhs)
  	case "&&" => And(rhs)
  	case "||" => Or(rhs)
  	case "&" => BitwiseAnd(rhs)
  	case "|" => BitwiseOr(rhs)
  	case "^" => BitwiseXor(rhs)
  	case "=" => Assign(rhs)
  }

  def ifThenElse:Parser[ExpressionTail] = for {
    //cond <- expression
    _ <- sep("?")
    trueClause <- expression
    _ <- sep(":")
    falseClause <- expression
  } yield IfThenElse(trueClause, falseClause)

  def ternaryExpressionTail:Parser[ExpressionTail] = ifThenElse

  def expressionHead:Parser[ExpressionHead] = anyAttempt(List(functionCall,  enclosedExpression, unaryOperation, delExpression, newExpression, prefixUnaryOperation, primaryExpression))

  def expressionTail:Parser[ExpressionTail] = anyAttempt(List(indexAccessTail, memberAccessTail, methodCallTail, unaryOperationTail, binaryOperationTail, ternaryExpressionTail, commaTail))

  def expression:Parser[Expression] = for {
    head <- expressionHead
    tail <- optional(expressionTail)
  } yield Expression(head, toOption(tail))

  // storage location

  def memory:Parser[StorageLocation] = for { _ <- spaceString("memory") } yield Memory
  def storage:Parser[StorageLocation] = for { _ <- spaceString("storage") } yield Storage
  def storageLocation:Parser[StorageLocation] = any(List(memory, storage))

  // types

  def typeName:Parser[TypeName] = anyAttempt(List(elementaryTypeName, storageLocationTypeName, mapping, arrayTypeName))

  def addressType:Parser[ElementaryTypeName] = for { _ <- string("address") } yield AddressType
  def boolType:Parser[ElementaryTypeName] = for { _ <- string("bool") } yield BoolType
  def stringType:Parser[ElementaryTypeName] = for { _ <- string("string") } yield StringType
  def varType:Parser[ElementaryTypeName] = for { _ <- string("var") } yield VarType
  def intType:Parser[ElementaryTypeName] = for { s <- prefixed(string("int"))(digits) } yield IntType(s)
  def uintType:Parser[ElementaryTypeName] = for { s <- prefixed(string("uint"))(digits) } yield UintType(s)
  def byteType:Parser[ElementaryTypeName] = for { s <- prefixed(string("byte"))(prefixed(string("s"))(digits)) } yield ByteType(s)
  def fixedType:Parser[ElementaryTypeName] = for { s <- prefixed(string("fixed"))(xdigits) } yield FixedType(s)
  def ufixedType:Parser[ElementaryTypeName] = for { s <- prefixed(string("ufixed"))(xdigits) } yield UfixedType(s)

  def elementaryType:Parser[ElementaryTypeName] = 
    anyAttempt(List(addressType,boolType,stringType,varType, intType, uintType, byteType, fixedType, ufixedType)) 

  def elementaryTypeName:Parser[TypeName] = for {
    _ <- elementaryType
  } yield ElementaryType

  def storageLocationTypeName:Parser[TypeName] = for {
    id <- identifier
    _ <- whiteSpace1
    loc <- optional(storageLocation)
  } yield StorageLocationTypeName(id, toOption(loc))

  def mapping:Parser[TypeName] = for {
    _ <- string("mapping")
    _ <- sep("(")
    elemType <- elementaryType
    _ <- sep("=>")
    name <- typeName
    _ <- sep(")")
  } yield Mapping(elemType, name)

  def arrayTypeName:Parser[TypeName] = for {
    name <- typeName
    storage <- optional(storageLocation)
    _ <- whiteSpace1
    _ <- sep("[")
    exp <- optional(expression)
    _ <- sep("]")
  } yield ArrayTypeName(name, toOption(storage), toOption(exp))

  def variableDeclaration:Parser[VariableDeclaration] = for {
    name <- typeName
    _ <- whiteSpace1
    id <- identifier
  } yield VariableDeclaration(name, id)

  // block
  def blockStatement:Parser[Block] = for {
    _ <- sep("{")
    stmt <- many(statement)
    _ <- sep("}")
  } yield BlockStatement(stmt)

  def block:Parser[Statement] = for {
    b <- blockStatement
  } yield b

  // statements

  def statement:Parser[Statement] = anyAttempt(List(ifStatement, whileStatement, forStatement, block, placeHolderStatement, semicolonStatement))

  def ifStatement:Parser[Statement] = {
    def elseStmt:Parser[Statement] = for {
      _ <- string("else")
      _ <- whiteSpace1
      stmt <- statement
    } yield stmt

    for {
      _ <- sep("if")
      _ <- sep("(")
      cond <- expression
      _ <- sep(")")
      ifClause <- statement
      elseClause <- optional(elseStmt)
    } yield IfStatement(cond, ifClause, toOption(elseClause))
  }

  def whileStatement:Parser[Statement] = for {
    _ <- string("while")
    _ <- sep("(")
    cond <- expression
    _ <- sep(")")
    body <- statement
  } yield WhileStatement(cond, body)

  def variableDefinition:Parser[VariableDefinition] = {
    def variableAssignment:Parser[Expression] = for {
      _ <- sep("=")
      exp <- expression
    } yield exp

    for {
      decl <- variableDeclaration
      assign <- optional(variableAssignment)
    } yield VariableDefinition(decl, toOption(assign))
  }

  def expressionStatement:Parser[Statement] = for {
    exp <- expression
  } yield ExpressionStatement(exp)

  def simpleStatement:Parser[Statement] = for {
    x <- varDefOrExpression
  } yield SimpleStatement(x)

  def varDefOrExpression:Parser[VarDefOrExpression] = for {
    x <- either1(variableDefinition)(expression)
  } yield toEither(x) 

  def forStatement:Parser[Statement] = for {
    _ <- string("for")
    _ <- sep("(")
    initStmt <- optional(varDefOrExpression)
    _ <- sep(";")
    cond <- optional(expression)
    _ <- sep(";")
    step <- optional(expression)
    _ <- sep(")")
    body <- statement
  } yield ForStatement(toOption(initStmt), toOption(cond), toOption(step), body)

  def placeHolderStatement:Parser[Statement] = for {
    _ <- char('_')
  } yield PlaceHolderStatement

  def continueStatement:Parser[Statement] = for {
    _ <- string("continue")
  } yield ContinueStatement

  def breakStatement:Parser[Statement] = for {
    _ <- string("break")
  } yield BreakStatement

  def throwStatement:Parser[Statement] = for {
    _ <- string("throw")
  } yield ThrowStatement

  def returnStatement:Parser[Statement] = for {
    _ <- string("return")
    _ <- whiteSpace1
    exp <- optional(expression)
  } yield ReturnStatement(toOption(exp))

  // ( Continue | Break | Return | Throw | VariableDefinition | Expression ) ';'
  def semicolonStatement:Parser[Statement] = for {
    stmt <- anyAttempt(List(continueStatement, breakStatement, returnStatement, throwStatement, simpleStatement, expressionStatement))
    _ <- sep(";")
  } yield stmt

  // parameter/list

  def parameter:Parser[Parameter] = for {
    typeName <- typeName   
    indexed <- optional(spaceString("indexed"))
    id <- optional(spaceSeq(identifier))
  } yield Parameter(typeName, toOption(id), isPresent(indexed))

  def parameterList:Parser[ParameterList] = for {
    _ <- char('(')
    params <- interleave(parameter)(sep(","))
    _ <- char(')')
  } yield ParameterList(params)

  // function modifiers

  def functionCallModifier:Parser[FunctionCallModifier] = for {
    f <- functionCallExpr
  } yield FunctionCallModifier(f)

  def functionCallFM:Parser[FunctionModifier] = for {
    f <- functionCallModifier
  } yield f

  def identifierModifier:Parser[IdentifierModifier] = for {
    id <- identifier
  } yield IdentifierModifier(id)

  def identifierFM:Parser[FunctionModifier] = for {
    id <- identifierModifier
  } yield id

  def constantModifier:Parser[ConstantModifier] = for {
    _ <- string("constant")
  } yield ConstantModifier()

  def constantFM:Parser[FunctionModifier] = for {
    c <- constantModifier
  } yield c

  def externalModifier:Parser[ExternalModifier] = for {
    _ <- string("external")
  } yield ExternalModifier()

  def externalFM:Parser[FunctionModifier] = for {
    e <- externalModifier
  } yield e

  def publicModifier:Parser[PublicModifier] = for {
    _ <- string("public")
  } yield PublicModifier()

  def publicFM:Parser[FunctionModifier] = for {
    p <- publicModifier
  } yield p

  def publicAM:Parser[AccessModifier] = for {
    p <- publicModifier
  } yield p

  def internalModifier:Parser[InternalModifier] = for {
    _ <- string("internal")
  } yield InternalModifier()

  def internalFM:Parser[FunctionModifier] = for {
    i <- internalModifier
  } yield i

  def internalAM:Parser[AccessModifier] = for {
    i <- internalModifier
  } yield i

  def privateModifier:Parser[PrivateModifier] = for {
    _ <- string("private")
  } yield PrivateModifier()

  def privateFM:Parser[FunctionModifier] = for {
    p <- privateModifier
  } yield p

  def privateAM:Parser[AccessModifier] = for {
    p <- privateModifier
  } yield p

  def functionModifier:Parser[FunctionModifier] = 
    anyAttempt(List(functionCallFM, identifierFM, constantFM, externalFM, publicFM, internalFM, privateFM))

  def accessModifier:Parser[AccessModifier] = anyAttempt(List(publicAM, internalAM, privateAM))

  // contract parts

  def enumValue:Parser[EnumValue] = identifier

  def enumDefinition:Parser[ContractPart] = for {
    _ <- string("enum")
    id <- spaceSeq(identifier) 
    _ <- sep("{")
    values <- interleave(enumValue)(sep(","))
    _ <- sep("}")
  } yield EnumDefinition(id, values)

  def eventDefinition:Parser[ContractPart] = for {
    _ <- string("event")
    id <- spaceSeq(identifier) 
    params <- spaceSeq(parameterList)
    anon <- optional(spaceString("anonymous"))
    _ <- sep(";") 
  } yield EventDefinition(id, params, isPresent(anon))

  def functionDefinition:Parser[ContractPart] = {
    def returns:Parser[ParameterList] = for {
      _ <- whiteSpace1
      _ <- string("returns")
      _ <- whiteSpace1
      params <- parameterList
    } yield params

    for {
      _ <- string("function") 
      id <- optional(spaceSeq(identifier))
      params <- parameterList
      modifiers <- optional(interleave(functionModifier)(whiteSpace1))
      returns <- optional(returns)
      _ <- whiteSpaces
      body <- blockStatement
    } yield FunctionDefinition(toOption(id), 
                               params, 
                               toOption(modifiers).getOrElse(List()), 
                               toOption(returns).getOrElse(ParameterList(List())), 
                               body)
  }

  def modifierDefinition:Parser[ContractPart] = for {
    _ <- string("modifier")
    _ <- whiteSpace1
    id <- identifier
    params <- optional(spaceSeq(parameterList))
    _ <- whiteSpaces
    block <- blockStatement
  } yield ModifierDefinition(id, toOption(params).getOrElse(ParameterList(List())), block)

  def structDefinition:Parser[ContractPart] = for {
    _ <- string("struct")
    _ <- whiteSpace1
    id <- identifier
    _ <- sep("{")
    vars <- many(seq(variableDeclaration, sep(";")))
    _ <- sep("}")
  } yield StructDefinition(id, vars.map(_._1))

  def usingForDeclaration:Parser[ContractPart] = for {
    _ <- string("struct")
    _ <- whiteSpace1
    id <- identifier
    _ <- whiteSpace1
    _ <- string("for") 
    _ <- whiteSpace1
    typeNameOrWildcard <- either1(typeName)(char('*'))
    _ <- spaceString(";")
  } yield UsingForDeclaration(id, toOptionLeft(typeNameOrWildcard))

  def stateVariableDeclaration:Parser[ContractPart] = for {
    typeName <- typeName
    am <- optional(spaceSeq(accessModifier))
    id <- spaceSeq(identifier)
    exp <- optional(seq(sep("="), expression))
    _ <- whiteSpaces
    _ <- char(';')
  } yield StateVariableDeclaration(typeName, toOption(am), id, toOption(exp).map(_._2))

  def contractPart:Parser[ContractPart] = 
    anyAttempt(List(stateVariableDeclaration, usingForDeclaration, 
                    structDefinition, modifierDefinition, functionDefinition, 
                    eventDefinition, enumDefinition))

  // contract definition

  def inheritanceSpecifier:Parser[InheritanceSpecifier] = for {
    name <- identifier
    args <- optional(functionCallArgs)
  } yield InheritanceSpecifier(name, toOption(args).getOrElse(List()))

  def contractDefinition:Parser[ContractDefinition] = {
    def inheritance:Parser[List[InheritanceSpecifier]] = for {
      _ <- whiteSpace1
      _ <- string("is")
      _ <- whiteSpace1
      list <- interleave(inheritanceSpecifier)(sep(","))
    } yield list

    for {
      defType <- any(List(string("library"), string("contract")))
      _ <- whiteSpace1
      id <- identifier
      inheritance <- optional(inheritance)
      parts <- many(contractPart)
      _ <- sep("}")
    } yield defType match {
      case "library" => LibraryDef(id, toOption(inheritance).getOrElse(List()), parts)
      case "contract" => ContractDef(id, toOption(inheritance).getOrElse(List()), parts)
    }
  }
}
