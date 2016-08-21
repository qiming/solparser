package com.cloakapps

import scalaz._
import Scalaz.{interleave => _, _}
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

	def sourceUnit:Parser[SourceUnit] = point(Nil)

}
