package com.cloakapps

import scalaz._
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.CharParser.{
  sat => psat, everythingUntil => peverythingUntil,  
  lookAhead => plookAhead, getState => pgetState, setState => psetState, 
  point => ppoint, item => pitem, _
} // p for polymorphic

object SolParserPrimitive {

  case class State(line:Int)
  val initState = State(1)

  def incrLine(s:(State,List[Token])):(State,List[Token]) = s match 
  {
    case (State(l),tks) => (State(l+1),tks)
  }

  def item:Parser[State,Token] = for 
  {
    c  <- pitem[State]
    _ <- incrLineIfRet(c)
  } yield c

  def sat(p: Token => Boolean): Parser[State,Token] = for 
  {
    c <- psat[State](p)
    _ <- incrLineIfRet(c)
  } yield c

  
  def everythingUntil(p: Token => Boolean): Parser[State,List[Token]] = for 
  {
    cs <- peverythingUntil[State](p)
    _  <- cs.map( (c:Token) => incrLineIfRet(c)).sequence[({type l[A]=Parser[State,A]})#l,Unit]
  } yield cs
  def lookAhead[A](p: Parser[State,A]): (Parser[State,A]) = plookAhead[State,A](p)
  def getState: Parser[State,(State,List[Token])] = pgetState
  def setState(st_toks: (State,List[Token])): Parser[State,Unit] = psetState(st_toks)
  def point[A](x:A):Parser[State,A] = ppoint(x)

  def incrLineIfRet(c:Token) : Parser[State,Unit] = 
  {
    if (c == '\n') 
    { 
      for 
      { 
        st <- getState
        _  <- setState(incrLine(st))
      } yield ()
    } 
    else 
    {
      point(())
    }
  } 
  // same as token from scalazparsec 0.1.1
  def char(x:Token):Parser[State,Token] = sat( ((y:Token) => y == x) )

  def anyChar:Parser[State,Token] = item

  def oneOf(s:String):Parser[State,Token] =
  {
    val l = s.toSet
    sat(l.contains(_))
  }

  def noneOf(s:String):Parser[State,Token] =
  {
    val l = s.toSet
    sat(!l.contains(_))
  }

  def eof:Parser[State,Unit] = for
  {
    x <- notFollowedBy(item)
  } yield(x)

  def notFollowedBy[A](p:Parser[State,A]):Parser[State,Unit] =
  {
    def t:Parser[State,Unit] = for
    {
      c <- attempt(p)
      x <- (unexpected:Parser[State,Unit])
    } yield(x)
    def t2:Parser[State,Unit] = point(())
    attempt(+++(t)(t2))
  }

  def unexpected[A](implicit m:MonadPlus[({type Lambda[B] = Parser[State,B]})#Lambda]): Parser[State,A] = m.empty

  def between[A,B,C](p1:Parser[State,A],p2:Parser[State,B],p3:Parser[State,C]):Parser[State,C] = for
  {
    _ <- p1
    x <- p3
    _ <- p2
  } yield x

  def option[A](a:A,p:Parser[State,A]) =
    +++(p)(point(a))

  def when(cond:Boolean)(p:Parser[State,Unit]):Parser[State,Unit] =
  {
    if (cond)
    {
      p
    }
    {
      point(())
    }
  }

  def guard(cond:Boolean)(implicit m:MonadPlus[({type Lambda[B] = Parser[State,B]})#Lambda]):Parser[State,Unit] =
  {
    if (cond)
    { point(()) }
    else
    { m.empty }
  }


  def string(s:String):Parser[State,String] =
  {
    def string_(l:List[Char]):Parser[State,List[Char]] = l match
    {
      case Nil => point(Nil)
      case (c::cs) => for
      {
        _ <- sat (_ == c)
        _ <- string_(cs)
      } yield (c::cs)
    }

    for
    {
      l <- string_(s.toList)
    } yield l.mkString
  }

  def digit:Parser[State,Char] = sat (_.isDigit)

  def isWhiteSpace(c:Char):Boolean = c match
  {
    case ' ' => true
    case '\t' => true
    case '\n' => true
    case '\r' => true
    case _ => false
  }

  def whiteSpace:Parser[State,Char] = sat(x => isWhiteSpace(x))

  def seq[A,B](pa:Parser[State,A],pb:Parser[State,B]):Parser[State,(A,B)] = for
  {
    a <- pa
    b <- pb
  } yield (a,b)

  //def whiteSpaces:Parser[List[Char]] = everythingUntil(!isWhiteSpace(_))
  def whiteSpaces:Parser[State,List[Char]] = many(whiteSpace)


  def whiteSpace1:Parser[State,List[Char]] = for {
    a <- whiteSpace
    b <- whiteSpaces
  } yield a::b

  // string prefixed by zero or more spaces
  def spaceString(s: String):Parser[State,String] = for {
    _ <- whiteSpaces
    s <- string(s)
  } yield s

  // something surrounded by optional spaces
  def spacesAround[A](separator:Parser[State,A]):Parser[State,A] = between(whiteSpaces, whiteSpaces, separator)
  // string surrounded by optional spaces
  def sep(separator:String):Parser[State,String] = spacesAround(string(separator))

  // string prefixed by another string
  def prefixed(prefix:Parser[State,String])(tail:Parser[State,String]):Parser[State,String] = for {

  	(x,xs) <- seq(prefix, optional(tail))
  } yield x + toOption(xs).getOrElse("")

  def digits:Parser[State,String] = for {
  	s <- many1(digit)
	} yield s.mkString

	// repeat pa exactly n times.
	def repeat[A](n:Int)(pa:Parser[State,A]):Parser[State,List[A]] = 
		if (n <= 0) point(List())
		else for {
			x <- pa
			xs <- repeat(n-1)(pa)
		} yield x::xs

	def hexChar:Parser[State,Char] = sat(x => x.isDigit || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

	def hex:Parser[State,String] = {
		for {
			x <- many(repeat(2)(hexChar))
		} yield x.flatMap(x => x).mkString
	}

	// strings like "24x1234"
  def xdigits:Parser[State,String] = for {
  	a <- many1(digit)
  	x <- char('x')
  	s <- many1(digit)
	} yield (a::x::s).mkString

  def toOption[A](a: \/[A,Unit]): Option[A] = a match {
    case -\/(x) => Some(x) // left
    case \/-(_)  => None   // right
  }

  def toOptionLeft[A,B](a: \/[A,B]): Option[A] = a match {
    case -\/(x) => Some(x) // left
    case \/-(_)  => None   // right
  }

  def toOptionRight[A,B](a: \/[A,B]): Option[B] = a match {
    case -\/(_) => None 			// left
    case \/-(x)  => Some(x)   // right
  }

  def isPresent[A](a: \/[A,Unit]): Boolean = toOption(a) != None

  def toEither[A,B](a: \/[A,B]): Either[A,B] = a match {
    case -\/(x) => Left(x) 
    case \/-(x) => Right(x)   
  }

  def any[A](parsers: List[Parser[State,A]]):Parser[State,A] = parsers.reduceLeft((a,b) => +++(a)(b))
  def anyAttempt[A](parsers: List[Parser[State,A]]):Parser[State,A] = parsers.reduceLeft((a,b) => +++(attempt(a))(attempt(b)))
}
