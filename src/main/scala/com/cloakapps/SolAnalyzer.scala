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
import com.cloakapps.CommentRemover._
import com.cloakapps.SolParser._


/**
  * Created by Qiming Li on 8/20/2016.
  */
object SolAnalyzer {

  def analyzeSolFile(path:String) {
    parseSolFile(path) match {
      case Consumed(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString+ "'")
      case Consumed(Succ((a,(st,tokens)))) if tokens.length > 0 =>  println("Parsing incomplete.")
      case Consumed(Succ((a,(st,tokens)))) =>  println(a.toString)
      case Empty(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString + "'")
      case otherwise => println(otherwise)
    }
  }
}
