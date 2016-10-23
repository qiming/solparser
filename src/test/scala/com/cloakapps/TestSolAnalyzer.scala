package com.cloakapps

import com.cloakapps.SolidityAST._
import com.cloakapps.SolParser._
import com.cloakapps.SolParserPrimitive._
import com.cloakapps.CommentRemover._
import com.cloakapps.SolAnalyzer._
import com.github.luzhuomi.scalazparsec.CharParser._

object TestAnalyzer extends App {
  args.foreach(file => analyzeSolFile(file))
}