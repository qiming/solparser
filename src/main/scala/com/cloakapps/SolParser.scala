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


	def identifier:Parser[Identifier] = for 
	{
		c  <- sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z'))
		cs <- many( sat ( x => (x >= 'a' && x <= 'z') || x == '_' || (x >= 'A' && x<= 'Z') || x.isDigit ) ) 
	} yield (c::cs).mkString

	def stringLiteral:Parser[StringLiteral] = for
	{
    _ <- sat(x => x == ''' || x == '"')
		cs <- many(sat(x => x != '\r' && x != '\n' && x != '"' && x != ''')) // TODO , exclude \\a , handle escape
    _ <- sat(x => x == ''' || x == '"')
	} yield cs.mkString

  // booleans
	def trueLiteral:Parser[BooleanLiteral] = for {
    _ <- string("true")
  } yield True
	def falseLiteral:Parser[BooleanLiteral] = for {
    _ <- string("false")
  } yield False
	def booleanLiteral:Parser[BooleanLiteral] = +++(attempt(trueLiteral))(attempt(falseLiteral))

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
    _ <- many1(either1(char(' '))(char('\t')))
    u <- any(List(moneyUnit, timeUnit))
  } yield u

  def numberLiteral:Parser[NumberLiteral] = for {
    prefix <- either1(string("0x"))(digit)
    cs <- many(digit)
    maybeUnit <- optional(numberUnit)
    unit = maybeUnit match {
      case -\/(x) => Some(x) // left
      case \/-(_) => None    // right
    }
  } yield NumberLiteral(prefix::cs mkString, unit)
}
