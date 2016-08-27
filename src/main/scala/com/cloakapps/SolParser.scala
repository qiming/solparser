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

  def parseSol(x:String):Result[Option[(SourceUnit,List[Token])]] =
  {
    def m:Parser[SourceUnit] = for
    {
      su <- sourceUnit
      _   <- eof
    } yield (su)
    run(m)(x.toList)
  }

  def getLoc:Parser[Int] = for
  {
    tokens <- getState
  } yield tokens.length

  def sourceUnit:Parser[SourceUnit] = point(Nil) // TODO

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


  def identifier:Parser[Identifier] = for {
    c  <- sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z'))
    cs <- many( sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z') || x.isDigit ) )
  } yield (c::cs).mkString

  def identifierExpr:Parser[Expression] = for {
    s <- identifier
  } yield IdentifierExpr(s)

  def stringLiteral:Parser[StringLiteral] = for {
    _ <- sat(x => x == ''' || x == '"')
    cs <- many(sat(x => x != '\r' && x != '\n' && x != '"' && x != ''')) // TODO , exclude \\a , handle escape
    _ <- sat(x => x == ''' || x == '"')
  } yield cs.mkString

  def stringLiteralExpr:Parser[Expression] = for {
    s <- stringLiteral
  } yield StringLiteralExpr(s)

  // booleans
  def trueLiteral:Parser[Expression] = for {
    _ <- string("true")
  } yield True
  def falseLiteral:Parser[Expression] = for {
    _ <- string("false")
  } yield False
  def booleanLiteral:Parser[Expression] = anyAttempt(List(trueLiteral, falseLiteral))

  // numbers

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

  def numberUnit:Parser[NumberUnit] = for
  {
    _ <- whiteSpace1
    u <- any(List(moneyUnit, timeUnit))
  } yield u

  def numberLiteral:Parser[Expression] = for {
    prefix <- either1(string("0x"))(digit)
    cs <- many(digit)
    maybeUnit <- optional(numberUnit)
  } yield NumberLiteral((prefix::cs).mkString, toOption(maybeUnit))

  def primaryExpression:Parser[Expression] = anyAttempt(List(identifierExpr, booleanLiteral, numberLiteral, stringLiteralExpr))

  def bracedExpr:Parser[Expression] = for {
    _ <- sep("(")
    exp <- expression
    _ <- sep(")")
  } yield exp

  def functionCall:Parser[Expression] = empty // TODO

  def newExpression:Parser[Expression] = for {
    _ <- string("new")
    _ <- many1(whiteSpace)
    id <- identifier
  } yield NewExpression(id)

  def delExpression:Parser[Expression] = for {
    _ <- string("delete")
    _ <- many1(whiteSpace)
    exp <- expression
  } yield exp

  def memberAccess:Parser[Expression] = for {
    exp <- expression
    _ <- char('.')
    id <- identifier
  } yield MemberAccess(exp, id)

  def indexAccess:Parser[Expression] = for {
    exp <- expression
    _ <- sep("[")
    ind <- optional(expression)
    _ <- sep("]")
  } yield IndexAccess(exp, toOption(ind))

  def comma:Parser[Expression] = for {
    first <- optional(expression)
    _ <- sep(",")
    second <- expression
  } yield Comma(toOption(first), second)

  def postfixUnaryOp:Parser[String] = anyAttempt(List("++", "--", ">>", "<<").map(s => string(s)))
  def prefixUnaryOp:Parser[String] = anyAttempt(List("++", "--", "!", "~", "+", "-").map(s => string(s)))
  def binaryOp:Parser[String] = anyAttempt(List("**", 
  	"==", "!=", "<=", ">=", "<", ">", 
  	"*=", "/=", "%=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", 
  	"*", "/", "%", "+", "-", "&&", "||", "&", "|", "^", "=").map(s => string(s)))

  def postfixUnaryOperation:Parser[Expression] = for {
    exp <- expression
    op <- postfixUnaryOp
  } yield op match {
    case "++" => IncrementPostfix(exp)
    case "--" => DecrementPostfix(exp)
    case "<<" => UnaryShiftLeft(exp)
    case ">>" => UnaryShiftRight(exp)
  }

  def prefixUnaryOperation:Parser[Expression] = for {
  	op <- prefixUnaryOp
  	exp <- expression
  } yield op match {
  	case "++" => IncrementPrefix(exp)
  	case "--" => DecrementPrefix(exp)
  	case "!" => Negate(exp)
  	case "~" => BitwiseNegate(exp)
  	case "+" => UnaryPlus(exp)
  	case "-" => UnaryMinus(exp)
  }

  def unaryOperation:Parser[Expression] = anyAttempt(List(prefixUnaryOp, postfixUnaryOperation))

  def binaryOperation:Parser[Expression] = for {
  	lhs <- expression
  	op <- binaryOp
  	rhs <- expression
  } yield op match {
  	case "**" => Power(lhs, rhs)
  	case "==" => EqualTo(lhs, rhs)
  	case "!=" => NotEqual(lhs, rhs)
  	case "<=" => LessOrEqual(lhs, rhs)
  	case ">=" => GreaterOrEqual(lhs, rhs)
  	case "<"  => LessThan(lhs, rhs)
  	case ">"  => GreaterThan(lhs, rhs)
  	case "*=" => MultiplyAssign(lhs, rhs)
  	case "/=" => DivideAssign(lhs, rhs)
  	case "%=" => RemainderAssign(lhs, rhs)
  	case "|=" => BitwiseOrAssign(lhs, rhs)
  	case "^=" => BitwiseXorAssign(lhs, rhs)
  	case "&=" => BitwiseAndAssign(lhs, rhs)
  	case "<<=" => LeftShiftAssign(lhs, rhs)
  	case ">>=" => RightShiftAssign(lhs, rhs)
  	case "+=" => PlusAssign(lhs, rhs)
  	case "-=" => MinusAssign(lhs, rhs)
  	case "*" => Multiply(lhs, rhs)
  	case "/" => DivideBy(lhs, rhs)
  	case "%" => Remainder(lhs, rhs)
  	case "+" => Add(lhs, rhs)
  	case "-" => Subtract(lhs, rhs)
  	case "&&" => And(lhs, rhs)
  	case "||" => Or(lhs, rhs)
  	case "&" => BitwiseAnd(lhs, rhs)
  	case "|" => BitwiseOr(lhs, rhs)
  	case "^" => BitwiseXor(lhs, rhs)
  	case "=" => Assign(lhs, rhs)
  }

  def ifThenElse:Parser[Expression] = for {
    cond <- expression
    _ <- sep("?")
    trueClause <- expression
    _ <- sep(":")
    falseClause <- expression
  } yield IfThenElse(cond, trueClause, falseClause)

  def ternaryExpression:Parser[Expression] = ifThenElse

  def expression:Parser[Expression] = anyAttempt(List(postfixUnaryOperation, functionCall, indexAccess, memberAccess, bracedExpr,
  	unaryOperation, binaryOperation, ternaryExpression, comma, primaryExpression)) 

}
