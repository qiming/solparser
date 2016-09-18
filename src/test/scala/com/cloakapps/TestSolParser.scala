package com.cloakapps

import com.cloakapps.SolidityAST._
import com.cloakapps.SolParser._
import com.cloakapps.SolParserPrimitive._
import com.cloakapps.CommentRemover._
import com.github.luzhuomi.scalazparsec.CharParser._

object ResultPrinter {
  def print[A](r:Result[ParseResult[State,(A,(State,List[Token]))]]) = r match {
    case Consumed(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString+ "'")
    case Consumed(Succ((a,(st,tokens)))) if tokens.length > 0 =>  println("Parsing incomplete.")
    case Consumed(Succ((a,(st,tokens)))) =>  println(a.toString)
    case Empty(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString + "'")
    case otherwise => println(otherwise)
  }
}

object TestStrip extends App {
  val s = List("contract // comment", 
               "some /* other comments */", 
               "and we have /* more", 
               "and inside comments", 
               " comments to strip */ until here.") 
  val t = strip(s);
  println(t)
}

object TestFile extends App {
  args.foreach(file => ResultPrinter.print(parseSolFile(file)))
}

object TestPart1 extends App {
  var s = """ function () returns (bool success);"""
  val r = parse(functionDeclaration)(s)
  ResultPrinter.print(r)
}

object TestParts extends App {
  var s = """ 
     event ProposalAdded(
        uint indexed proposalID,
        address recipient,
        uint amount,
        bool newCurator,
        string description
    );
"""
  val r = parse(eventDefinition)(s)
  ResultPrinter.print(r)
}

object TestSolParser extends App 
{
	val proStr = """
contract person {
}

contract mortal {
    address owner;

    function mortal() { owner = msg.sender; }

    function kill() { if (msg.sender == owner) selfdestruct(owner); }
}

contract greeter is mortal {
    string greeting;

    function greeter(string _greeting) public {
        greeting = _greeting;
    }

    function greet() constant returns (string) {
        return greeting;
    }
}
	"""
	println(proStr)
	val r = parseSol(proStr)
  ResultPrinter.print(r)
}