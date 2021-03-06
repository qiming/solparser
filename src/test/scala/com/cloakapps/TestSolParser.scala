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
  var s = """ 
        if (lastTimeOfNewCredit + TWELVE_HOURS < block.timestamp) {
            // Return money to sender
            msg.sender.send(amount);
            // Sends all contract money to the last creditor
            creditorAddresses[creditorAddresses.length - 1].send(profitFromCrash);
            corruptElite.send(this.balance);
            // Reset contract state
            lastCreditorPayedOut = 0;
            lastTimeOfNewCredit = block.timestamp;
            profitFromCrash = 0;
            creditorAddresses = new address[](0);
            creditorAmounts = new uint[](0);
            round += 1;
            return false;
        }
  """
  val r = parse(ifStatement)(s)
  ResultPrinter.print(r)
}

object TestPart2 extends App {
  var s = """ 
            new address[](0)
   """
  val r = parse(newExpression)(s)
  ResultPrinter.print(r)
}

object TestParts extends App {
  var s = """ 
    function lendGovernmentMoney(address buddy) returns (bool) {
        uint amount = msg.value;
        // check if the system already broke down. If for 12h no new creditor gives new credit to the system it will brake down.
        // 12h are on average = 60*60*12/12.5 = 3456
        if (lastTimeOfNewCredit + TWELVE_HOURS < block.timestamp) {
            // Return money to sender
            msg.sender.send(amount);
            // Sends all contract money to the last creditor
            creditorAddresses[creditorAddresses.length - 1].send(profitFromCrash);
            corruptElite.send(this.balance);
            // Reset contract state
            lastCreditorPayedOut = 0;
            lastTimeOfNewCredit = block.timestamp;
            profitFromCrash = 0;
            creditorAddresses = new address[](0);
            creditorAmounts = new uint[](0);
            round += 1;
            return false;
        }
        else {
            // the system needs to collect at least 1% of the profit from a crash to stay alive
            if (amount >= 10 ** 18) {
                // the System has received fresh money, it will survive at leat 12h more
                lastTimeOfNewCredit = block.timestamp;
                // register the new creditor and his amount with 10% interest rate
                creditorAddresses.push(msg.sender);
                creditorAmounts.push(amount * 110 / 100);
                // now the money is distributed
                // first the corrupt elite grabs 5% - thieves!
                corruptElite.send(amount * 5/100);
                // 5% are going into the economy (they will increase the value for the person seeing the crash comming)
                if (profitFromCrash < 10000 * 10**18) {
                    profitFromCrash += amount * 5/100;
                }
                // if you have a buddy in the government (and he is in the creditor list) he can get 5% of your credits.
                // Make a deal with him.
                if(buddies[buddy] >= amount) {
                    buddy.send(amount * 5/100);
                }
                buddies[msg.sender] += amount * 110 / 100;
                // 90% of the money will be used to pay out old creditors
                if (creditorAmounts[lastCreditorPayedOut] <= address(this).balance - profitFromCrash) {
                    creditorAddresses[lastCreditorPayedOut].send(creditorAmounts[lastCreditorPayedOut]);
                    buddies[creditorAddresses[lastCreditorPayedOut]] -= creditorAmounts[lastCreditorPayedOut];
                    lastCreditorPayedOut += 1;
                }
                return true;
            }
            else {
                msg.sender.send(amount);
                return false;
            }
        }
    }
"""
  val r = parse(functionDefinition)(s)
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