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
		cs <- many1(sat(!isWhiteSpace(_))) // TODO , exclude \\a
	} yield cs.mkString


}
