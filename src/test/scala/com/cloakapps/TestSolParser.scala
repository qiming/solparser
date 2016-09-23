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
  var s = """ if (quorum >= minQuorum(p.amount) && p.yea > p.nay && proposalCheck) {
            if (!p.recipient.call.value(p.amount)(_transactionData))
                throw;

            p.proposalPassed = true;
            _success = true;

            // only create reward tokens when ether is not sent to the DAO itself and
            // related addresses. Proxy addresses should be forbidden by the curator.
            if (p.recipient != address(this) && p.recipient != address(rewardAccount)
                && p.recipient != address(DAOrewardAccount)
                && p.recipient != address(extraBalance)
                && p.recipient != address(curator)) {

                rewardToken[address(this)] += p.amount;
                totalRewardToken += p.amount;
            }
        }
   """
  val r = parse(ifStatement)(s)
  ResultPrinter.print(r)
}

object TestPart2 extends App {
  var s = """ uint constant creationGracePeriod = 40 days;
   """
  val r = parse(stateVariableDeclaration)(s)
  ResultPrinter.print(r)
}

object TestParts extends App {
  var s = """ 
     function executeProposal(
        uint _proposalID,
        bytes _transactionData
    ) noEther returns (bool _success) {

        Proposal p = proposals[_proposalID];

        uint waitPeriod = p.newCurator
            ? splitExecutionPeriod
            : executeProposalPeriod;
        // If we are over deadline and waiting period, assert proposal is closed
        if (p.open && now > p.votingDeadline + waitPeriod) {
            closeProposal(_proposalID);
            return;
        }

        // Check if the proposal can be executed
        if (now < p.votingDeadline  // has the voting deadline arrived?
            // Have the votes been counted?
            || !p.open
            // Does the transaction code match the proposal?
            || p.proposalHash != sha3(p.recipient, p.amount, _transactionData)) {

            throw;
        }

        // If the curator removed the recipient from the whitelist, close the proposal
        // in order to free the deposit and allow unblocking of voters
        if (!isRecipientAllowed(p.recipient)) {
            closeProposal(_proposalID);
            p.creator.send(p.proposalDeposit);
            return;
        }

        bool proposalCheck = true;

        if (p.amount > actualBalance())
            proposalCheck = false;

        uint quorum = p.yea + p.nay;

        // require 53% for calling newContract()
        if (_transactionData.length >= 4 && _transactionData[0] == 0x68
            && _transactionData[1] == 0x37 && _transactionData[2] == 0xff
            && _transactionData[3] == 0x1e
            && quorum < minQuorum(actualBalance() + rewardToken[address(this)])) {

                proposalCheck = false;
        }

        if (quorum >= minQuorum(p.amount)) {
            if (!p.creator.send(p.proposalDeposit))
                throw;

            lastTimeMinQuorumMet = now;
            // set the minQuorum to 20% again, in the case it has been reached
            if (quorum > totalSupply / 5)
                minQuorumDivisor = 5;
        }

        // Execute result
        if (quorum >= minQuorum(p.amount) && p.yea > p.nay && proposalCheck) {
            if (!p.recipient.call.value(p.amount)(_transactionData))
                throw;

            p.proposalPassed = true;
            _success = true;

            // only create reward tokens when ether is not sent to the DAO itself and
            // related addresses. Proxy addresses should be forbidden by the curator.
            if (p.recipient != address(this) && p.recipient != address(rewardAccount)
                && p.recipient != address(DAOrewardAccount)
                && p.recipient != address(extraBalance)
                && p.recipient != address(curator)) {

                rewardToken[address(this)] += p.amount;
                totalRewardToken += p.amount;
            }
        }

        closeProposal(_proposalID);

        // Initiate event
        ProposalTallied(_proposalID, _success, quorum);
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