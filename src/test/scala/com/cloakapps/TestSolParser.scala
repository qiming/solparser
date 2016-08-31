package com.cloakapps

import com.cloakapps.SolidityAST._
import com.cloakapps.SolParser._
import com.github.luzhuomi.scalazparsec.NonBacktracking._

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
		case Empty(_) => println("parser failed to start.")
		case Consumed(None) => println("parser aborted abnormally.")
		case Consumed(Some((source_unit,tokens))) if tokens.length > 0 =>  println("parser incompleted.")
		case Consumed(Some((source_unit,tokens))) =>  println(source_unit.toString)

	}
}