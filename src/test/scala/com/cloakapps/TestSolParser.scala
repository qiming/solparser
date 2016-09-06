package com.cloakapps

import com.cloakapps.SolidityAST._
import com.cloakapps.SolParser._
import com.cloakapps.SolParserPrimitive._
import com.github.luzhuomi.scalazparsec.CharParser._

object TestSolParser extends App 
{
	val proStr = """
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
	parseSol(proStr) match
	{
		case Consumed(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString+ "'")
		case Consumed(Succ((source_unit,(st,tokens)))) if tokens.length > 0 =>  println("Parsing incompleted.")
		case Consumed(Succ((source_unit,(st,tokens)))) =>  println(source_unit.toString)
		case Empty(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString + "'")
		case otherwise => println(otherwise)

	}
}