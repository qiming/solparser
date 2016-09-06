package com.cloakapps


import scalaz.{State => _, _}
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.CharParser.{
  sat => psat, everythingUntil => peverythingUntil,  
  lookAhead => plookAhead, getState => pgetState, setState => psetState, 
  point => ppoint, item => pitem, _
} // p for polymorphic
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
   
  def parse[A](pa:Parser[State,A])(x:String):Result[ParseResult[State,(A,(State,List[Token]))]] = {
    def m:Parser[State,A] = for {
      a <- pa
      _ <- eof
    } yield a
    run(m)(initState,x.toList)
  }

  def parseSol(x:String):Result[ParseResult[State, (SourceUnit,(State,List[Token]))]] = parse(sourceUnit)(x)

  def getLoc:Parser[State,Int] = for
  {
    tokens <- getState
  } yield tokens.length

  def sourceUnit:Parser[State,SourceUnit] = for {
    _     <- whiteSpaces
    units <- many(either1(importDirective)(contractDefinition))
  } yield units.map(toEither(_))

  def importDirective:Parser[State,ImportDirective] = +++(attempt(simpleImport))(+++(attempt(fromImport))(multipleImport))


  // +++(p1)(p2) execute p1 if it can proceed, otherwise p2
  // note that the parser combinator are by-default not back-tracking.
  // since simple , from and multiple imports are all starting with the keyword "import", we need to insert attempt()
  // to make the alternatives backtrackable.


  def simpleImport:Parser[State,ImportDirective] = for
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


  def fromImport:Parser[State,ImportDirective] = for
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


  def multipleImport:Parser[State,ImportDirective] =
  {
    def idAsIdOpt:Parser[State,(Identifier,Option[Identifier])] = for
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

  def asIdentifier:Parser[State,Identifier] = for
  {
    _  <- string("as")
    _  <- whiteSpaces
    id <- identifier
  } yield (id)

  // expressions

  def identifier:Parser[State,Identifier] = for {
    c  <- sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z'))
    cs <- many( sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z') || x.isDigit ) )
  } yield (c::cs).mkString

  def identifierExpr:Parser[State,ExpressionHead] = for {
    s <- identifier
  } yield IdentifierExpr(s)

  def hexLiteral:Parser[State,StringLiteral] = for {
    _ <- string("hex")
    open <- sat(x => x == '\'' || x == '"')
    h <- hex
    _ <- char(open)
  } yield "0x" + h

  def quotedLiteral:Parser[State,StringLiteral] = {
    def escaped:Parser[State,List[Char]] = for {
      s <- char('\\')
      c <- item
    } yield List(s,c)

    def normal(c:Char):Parser[State,List[Char]] = for {
      c <- sat(x => x != '\r' && x != '\n' && x != c)
    } yield List(c)

    for {
        open <- sat(x => x == '\'' || x == '"')
        cs <- many(anyAttempt(List(escaped, normal(open))))
        _ <- char(open)
    } yield cs.flatten.mkString
  }

  def stringLiteral:Parser[State,StringLiteral] = anyAttempt(List(hexLiteral, quotedLiteral))

  def stringLiteralExpr:Parser[State,ExpressionHead] = for {
    s <- stringLiteral
  } yield StringLiteralExpr(s)

  // booleans
  def trueLiteral:Parser[State,ExpressionHead] = for {
    _ <- string("true")
  } yield True
  def falseLiteral:Parser[State,ExpressionHead] = for {
    _ <- string("false")
  } yield False
  def booleanLiteral:Parser[State,ExpressionHead] = anyAttempt(List(trueLiteral, falseLiteral))

  // money units
  def wei:Parser[State,NumberUnit] = for { _ <- string("wei") } yield Wei
  def szabo:Parser[State,NumberUnit] = for { _ <- string("szabo") } yield Szabo
  def finney:Parser[State,NumberUnit] = for { _ <- string("finney") } yield Finney
  def ether:Parser[State,NumberUnit] = for { _ <- string("ether") } yield Ether
  def moneyUnit:Parser[State,NumberUnit] = anyAttempt(List(wei, szabo, finney, ether))

  // time units
  def seconds:Parser[State,NumberUnit] = for { _ <- string("seconds") } yield Seconds
  def minutes:Parser[State,NumberUnit] = for { _ <- string("minutes") } yield Minutes
  def hours:Parser[State,NumberUnit] = for { _ <- string("hours") } yield Hours
  def days:Parser[State,NumberUnit] = for { _ <- string("days") } yield Days
  def weeks:Parser[State,NumberUnit] = for { _ <- string("weeks") } yield Weeks
  def years:Parser[State,NumberUnit] = for { _ <- string("years") } yield Years
  def timeUnit:Parser[State,NumberUnit] = anyAttempt(List(seconds, minutes, hours, days, weeks, years))

  def numberUnit:Parser[State,NumberUnit] = for {
    _ <- whiteSpace1
    u <- anyAttempt(List(moneyUnit, timeUnit))
  } yield u

  def numberLiteral:Parser[State,ExpressionHead] = for {
    prefix <- optional(string("0x"))
    cs <- many1(digit)
    maybeUnit <- optional(numberUnit)
  } yield NumberLiteral(toOption(prefix).getOrElse("") + cs.mkString, toOption(maybeUnit))

  def primaryExpression:Parser[State,ExpressionHead] = anyAttempt(List(identifierExpr, booleanLiteral, numberLiteral, stringLiteralExpr))

  def enclosedExpression:Parser[State,ExpressionHead] = for {
    _ <- sep("(")
    exp <- expression
    _ <- sep(")")
  } yield EnclosedExpression(exp)

  def functionCallArgs:Parser[State,List[Expression]] = for {
    _ <- whiteSpaces
    _ <- char('(')
    _ <- whiteSpaces
    args <- interleave(expression)(sep(","))
    _ <- whiteSpaces
    _ <- char(')')
  } yield args

  def functionCallExpr:Parser[State,FunctionCallExpr] = for {
  	name <- identifier
    args <- functionCallArgs
  } yield FunctionCallExpr(name, args)

  def functionCall:Parser[State,ExpressionHead] = for {
    f <- functionCallExpr
  } yield f

  def methodCallTail:Parser[State,ExpressionTailStart] = for {
    //obj <- expression
    _ <- spaceString(".")
    name <- identifier
    _ <- sep("(")
    args <- interleave(expression)(sep(","))
    _ <- sep(")")
  } yield MethodCallTail(name, args)

  def newExpression:Parser[State,ExpressionHead] = for {
    _ <- string("new")
    _ <- many1(whiteSpace)
    id <- identifier
  } yield NewExpression(id)

  def delExpression:Parser[State,ExpressionHead] = for {
    _ <- string("delete")
    _ <- many1(whiteSpace)
    exp <- expression
  } yield DeleteExpression(exp)

  def memberAccessTail:Parser[State,ExpressionTailStart] = for {
    //exp <- expression
    _ <- spaceString(".")
    id <- identifier
  } yield MemberAccessTail(id)

  def indexAccessTail:Parser[State,ExpressionTailStart] = for {
    //exp <- expression
    _ <- sep("[")
    ind <- optional(expression)
    _ <- sep("]")
  } yield IndexAccessTail(toOption(ind))

  def commaTail:Parser[State,ExpressionTailStart] = for {
    //first <- expression
    _ <- sep(",")
    second <- optional(expression)
  } yield CommaTail(toOption(second))

  def postfixUnaryOp:Parser[State,String] = anyAttempt(List("++", "--", ">>", "<<").map(s => string(s)))
  def prefixUnaryOp:Parser[State,String] = anyAttempt(List("++", "--", "!", "~", "+", "-").map(s => string(s)))
  def binaryOp:Parser[State,String] = anyAttempt(List("**", 
  	"==", "!=", "<=", ">=", "<", ">", 
  	"*=", "/=", "%=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", 
  	"*", "/", "%", "+", "-", "&&", "||", "&", "|", "^", "=").map(s => string(s)))

  def postfixUnaryOperation:Parser[State,ExpressionTailStart] = for {
    //exp <- expression
    _ <- whiteSpaces
    op <- postfixUnaryOp
  } yield op match {
    case "++" => IncrementPostfixTail
    case "--" => DecrementPostfixTail
    case "<<" => UnaryShiftLeftTail
    case ">>" => UnaryShiftRightTail
  }

  def prefixUnaryOperation:Parser[State,ExpressionHead] = for {
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

  def unaryOperation:Parser[State,ExpressionHead] = attempt(prefixUnaryOperation)
  def unaryOperationTail:Parser[State,ExpressionTailStart] = attempt(postfixUnaryOperation)

  def binaryOperationTail:Parser[State,ExpressionTailStart] = for {
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

  def ifThenElse:Parser[State,ExpressionTailStart] = for {
    //cond <- expression
    _ <- sep("?")
    trueClause <- expression
    _ <- sep(":")
    falseClause <- expression
  } yield IfThenElse(trueClause, falseClause)

  def ternaryExpressionTail:Parser[State,ExpressionTailStart] = ifThenElse

  def expressionHead:Parser[State,ExpressionHead] = anyAttempt(List(functionCall,  enclosedExpression, unaryOperation, delExpression, newExpression, prefixUnaryOperation, primaryExpression))

  def expressionTailStart:Parser[State,ExpressionTailStart] = anyAttempt(List(indexAccessTail, memberAccessTail, methodCallTail, unaryOperationTail, binaryOperationTail, ternaryExpressionTail, commaTail))

  def expressionTail:Parser[State,ExpressionTail] = for {
    start <- expressionTailStart
    next <- optional(expressionTail)
  } yield ExpressionTail(start, toOption(next))

  def expression:Parser[State,Expression] = for {
    head <- expressionHead
    tail <- optional(expressionTail)
  } yield Expression(head, toOption(tail))

  // storage location

  def memory:Parser[State,StorageLocation] = for { _ <- spaceString("memory") } yield Memory
  def storage:Parser[State,StorageLocation] = for { _ <- spaceString("storage") } yield Storage
  def storageLocation:Parser[State,StorageLocation] = any(List(memory, storage))

  // types

  def typeName:Parser[State,TypeName] = anyAttempt(List(elementaryTypeName, storageLocationTypeName, mapping, arrayTypeName))

  def addressType:Parser[State,ElementaryTypeName] = for { _ <- string("address") } yield AddressType
  def boolType:Parser[State,ElementaryTypeName] = for { _ <- string("bool") } yield BoolType
  def stringType:Parser[State,ElementaryTypeName] = for { _ <- string("string") } yield StringType
  def varType:Parser[State,ElementaryTypeName] = for { _ <- string("var") } yield VarType
  def intType:Parser[State,ElementaryTypeName] = for { s <- prefixed(string("int"))(digits) } yield IntType(s)
  def uintType:Parser[State,ElementaryTypeName] = for { s <- prefixed(string("uint"))(digits) } yield UintType(s)
  def byteType:Parser[State,ElementaryTypeName] = for { s <- prefixed(string("byte"))(prefixed(string("s"))(digits)) } yield ByteType(s)
  def fixedType:Parser[State,ElementaryTypeName] = for { s <- prefixed(string("fixed"))(xdigits) } yield FixedType(s)
  def ufixedType:Parser[State,ElementaryTypeName] = for { s <- prefixed(string("ufixed"))(xdigits) } yield UfixedType(s)

  def elementaryType:Parser[State,ElementaryTypeName] = 
    anyAttempt(List(addressType,boolType,stringType,varType, intType, uintType, byteType, fixedType, ufixedType)) 

  def elementaryTypeName:Parser[State,TypeName] = for {
    _ <- elementaryType
  } yield ElementaryType

  def storageLocationTypeName:Parser[State,TypeName] = for {
    id <- identifier
    _ <- whiteSpace1
    loc <- optional(storageLocation)
  } yield StorageLocationTypeName(id, toOption(loc))

  def mapping:Parser[State,TypeName] = for {
    _ <- string("mapping")
    _ <- sep("(")
    elemType <- elementaryType
    _ <- sep("=>")
    name <- typeName
    _ <- sep(")")
  } yield Mapping(elemType, name)

  def arrayTypeName:Parser[State,TypeName] = for {
    name <- typeName
    storage <- optional(storageLocation)
    _ <- whiteSpace1
    _ <- sep("[")
    exp <- optional(expression)
    _ <- sep("]")
  } yield ArrayTypeName(name, toOption(storage), toOption(exp))

  def variableDeclaration:Parser[State,VariableDeclaration] = for {
    name <- typeName
    _ <- whiteSpace1
    id <- identifier
  } yield VariableDeclaration(name, id)

  // block
  def blockStatement:Parser[State,Block] = for {
    _ <- sep("{")
    stmt <- many(statement)
    _ <- sep("}")
  } yield BlockStatement(stmt)

  def block:Parser[State,Statement] = for {
    b <- blockStatement
  } yield b

  // statements

  def statement:Parser[State,Statement] = anyAttempt(List(ifStatement, whileStatement, forStatement, block, placeHolderStatement, semicolonStatement))

  def ifStatement:Parser[State,Statement] = {
    def elseStmt:Parser[State,Statement] = for {
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

  def whileStatement:Parser[State,Statement] = for {
    _ <- string("while")
    _ <- sep("(")
    cond <- expression
    _ <- sep(")")
    body <- statement
  } yield WhileStatement(cond, body)

  def variableDefinition:Parser[State,VariableDefinition] = {
    def variableAssignment:Parser[State,Expression] = for {
      _ <- sep("=")
      exp <- expression
    } yield exp

    for {
      decl <- variableDeclaration
      assign <- optional(variableAssignment)
    } yield VariableDefinition(decl, toOption(assign))
  }

  def expressionStatement:Parser[State,Statement] = for {
    exp <- expression
  } yield ExpressionStatement(exp)

  def simpleStatement:Parser[State,Statement] = for {
    x <- varDefOrExpression
  } yield SimpleStatement(x)

  def varDefOrExpression:Parser[State,VarDefOrExpression] = for {
    x <- either1(variableDefinition)(expression)
  } yield toEither(x) 

  def forStatement:Parser[State,Statement] = for {
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

  def placeHolderStatement:Parser[State,Statement] = for {
    _ <- char('_')
  } yield PlaceHolderStatement

  def continueStatement:Parser[State,Statement] = for {
    _ <- string("continue")
  } yield ContinueStatement

  def breakStatement:Parser[State,Statement] = for {
    _ <- string("break")
  } yield BreakStatement

  def throwStatement:Parser[State,Statement] = for {
    _ <- string("throw")
  } yield ThrowStatement

  def returnStatement:Parser[State,Statement] = for {
    _ <- string("return")
    _ <- whiteSpace1
    exp <- optional(expression)
  } yield ReturnStatement(toOption(exp))

  // ( Continue | Break | Return | Throw | VariableDefinition | Expression ) ';'
  def semicolonStatement:Parser[State,Statement] = for {
    stmt <- anyAttempt(List(continueStatement, breakStatement, returnStatement, throwStatement, simpleStatement, expressionStatement))
    _ <- sep(";")
  } yield stmt

  // parameter/list

  def parameter:Parser[State,Parameter] = for {
    typeName <- typeName   
    indexed <- optional(spaceString("indexed"))
    id <- optional(spaceSeq(identifier))
  } yield Parameter(typeName, toOption(id), isPresent(indexed))

  def parameterList:Parser[State,ParameterList] = for {
    _ <- char('(')
    params <- interleave(parameter)(sep(","))
    _ <- char(')')
  } yield ParameterList(params)

  // function modifiers

  def functionCallModifier:Parser[State,FunctionCallModifier] = for {
    f <- functionCallExpr
  } yield FunctionCallModifier(f)

  def functionCallFM:Parser[State,FunctionModifier] = for {
    f <- functionCallModifier
  } yield f

  def identifierModifier:Parser[State,IdentifierModifier] = for {
    id <- identifier
  } yield IdentifierModifier(id)

  def identifierFM:Parser[State,FunctionModifier] = for {
    id <- identifierModifier
  } yield id

  def constantModifier:Parser[State,ConstantModifier] = for {
    _ <- string("constant")
  } yield ConstantModifier()

  def constantFM:Parser[State,FunctionModifier] = for {
    c <- constantModifier
  } yield c

  def externalModifier:Parser[State,ExternalModifier] = for {
    _ <- string("external")
  } yield ExternalModifier()

  def externalFM:Parser[State,FunctionModifier] = for {
    e <- externalModifier
  } yield e

  def publicModifier:Parser[State,PublicModifier] = for {
    _ <- string("public")
  } yield PublicModifier()

  def publicFM:Parser[State,FunctionModifier] = for {
    p <- publicModifier
  } yield p

  def publicAM:Parser[State,AccessModifier] = for {
    p <- publicModifier
  } yield p

  def internalModifier:Parser[State,InternalModifier] = for {
    _ <- string("internal")
  } yield InternalModifier()

  def internalFM:Parser[State,FunctionModifier] = for {
    i <- internalModifier
  } yield i

  def internalAM:Parser[State,AccessModifier] = for {
    i <- internalModifier
  } yield i

  def privateModifier:Parser[State,PrivateModifier] = for {
    _ <- string("private")
  } yield PrivateModifier()

  def privateFM:Parser[State,FunctionModifier] = for {
    p <- privateModifier
  } yield p

  def privateAM:Parser[State,AccessModifier] = for {
    p <- privateModifier
  } yield p

  def functionModifier:Parser[State,FunctionModifier] = 
    anyAttempt(List(functionCallFM, identifierFM, constantFM, externalFM, publicFM, internalFM, privateFM))

  def accessModifier:Parser[State,AccessModifier] = anyAttempt(List(publicAM, internalAM, privateAM))

  // contract parts

  def enumValue:Parser[State,EnumValue] = identifier

  def enumDefinition:Parser[State,ContractPart] = for {
    _ <- string("enum")
    id <- spaceSeq(identifier) 
    _ <- sep("{")
    values <- interleave(enumValue)(sep(","))
    _ <- sep("}")
  } yield EnumDefinition(id, values)

  def eventDefinition:Parser[State,ContractPart] = for {
    _ <- string("event")
    id <- spaceSeq(identifier) 
    params <- spaceSeq(parameterList)
    anon <- optional(spaceString("anonymous"))
    _ <- sep(";") 
  } yield EventDefinition(id, params, isPresent(anon))

  def functionDefinition:Parser[State,ContractPart] = {
    def returns:Parser[State,ParameterList] = for {
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

  def modifierDefinition:Parser[State,ContractPart] = for {
    _ <- string("modifier")
    _ <- whiteSpace1
    id <- identifier
    params <- optional(spaceSeq(parameterList))
    _ <- whiteSpaces
    block <- blockStatement
  } yield ModifierDefinition(id, toOption(params).getOrElse(ParameterList(List())), block)

  def structDefinition:Parser[State,ContractPart] = for {
    _ <- string("struct")
    _ <- whiteSpace1
    id <- identifier
    _ <- sep("{")
    vars <- many(seq(variableDeclaration, sep(";")))
    _ <- sep("}")
  } yield StructDefinition(id, vars.map(_._1))

  def usingForDeclaration:Parser[State,ContractPart] = for {
    _ <- string("struct")
    _ <- whiteSpace1
    id <- identifier
    _ <- whiteSpace1
    _ <- string("for") 
    _ <- whiteSpace1
    typeNameOrWildcard <- either1(typeName)(char('*'))
    _ <- spaceString(";")
  } yield UsingForDeclaration(id, toOptionLeft(typeNameOrWildcard))

  def stateVariableDeclaration:Parser[State,ContractPart] = for {
    typeName <- typeName
    am <- optional(spaceSeq(accessModifier))
    id <- spaceSeq(identifier)
    exp <- optional(seq(sep("="), expression))
    _ <- whiteSpaces
    _ <- char(';')
  } yield StateVariableDeclaration(typeName, toOption(am), id, toOption(exp).map(_._2))

  def contractPart:Parser[State,ContractPart] = 
    anyAttempt(List(stateVariableDeclaration, usingForDeclaration, 
                    structDefinition, modifierDefinition, functionDefinition, 
                    eventDefinition, enumDefinition))

  // contract definition

  def inheritanceSpecifier:Parser[State,InheritanceSpecifier] = for {
    name <- identifier
    args <- optional(functionCallArgs)
  } yield InheritanceSpecifier(name, toOption(args).getOrElse(List()))

  def contractDefinition:Parser[State,ContractDefinition] = {
    def inheritance:Parser[State,List[InheritanceSpecifier]] = for {
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
