package com.cloakapps

import com.cloakapps.SolidityAST._
import com.cloakapps.SolParser._
import com.cloakapps.SolParserPrimitive._
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

object TestParts extends App {
  var s = """
    address home;
    int count;
    function person() {
      home = msg.sender;
    }
  """
  println(s)
  val r = parse(many(contractPart))(s)
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