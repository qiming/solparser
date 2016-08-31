package com.cloakapps

import scalaz._
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.NonBacktracking._

object SolParserPrimitive {
  // same as token from scalazparsec 0.1.1
  def char(x:Token):Parser[Token] = sat( ((y:Token) => y == x) )


  def anyChar:Parser[Token] = item

  def oneOf(s:String):Parser[Token] =
  {
    val l = s.toSet
    sat(l.contains(_))
  }

  def noneOf(s:String):Parser[Token] =
  {
    val l = s.toSet
    sat(!l.contains(_))
  }

  def eof:Parser[Unit] = for
  {
    x <- notFollowedBy(item)
  } yield(x)

  def notFollowedBy[A](p:Parser[A]):Parser[Unit] =
  {
    def t:Parser[Unit] = for
    {
      c <- attempt(p)
      x <- (unexpected:Parser[Unit])
    } yield(x)
    def t2:Parser[Unit] = point(())
    attempt(+++(t)(t2))
  }

  def unexpected[A](implicit m:MonadPlus[Parser]): Parser[A] = m.empty

  def between[A,B,C](p1:Parser[A],p2:Parser[B],p3:Parser[C]) :Parser[C] = for
  {
    _ <- p1
    x <- p3
    _ <- p2
  } yield x

  def option[A](a:A,p:Parser[A]) =
    +++(p)(point(a))

  def when(cond:Boolean)(p:Parser[Unit]):Parser[Unit] =
  {
    if (cond)
    {
      p
    }
    {
      point(())
    }
  }

  def guard(cond:Boolean)(implicit m:MonadPlus[Parser]):Parser[Unit] =
  {
    if (cond)
    { point(()) }
    else
    { m.empty }
  }


  def string(s:String):Parser[String] =
  {
    def string_(l:List[Char]):Parser[List[Char]] = l match
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

  def digit:Parser[Char] = sat (_.isDigit)

  def isWhiteSpace(c:Char):Boolean = c match
  {
    case ' ' => true
    case '\t' => true
    case '\n' => true
    case '\r' => true
    case _ => false
  }

  def whiteSpace:Parser[Char] = sat (isWhiteSpace)

  def seq[A,B](pa:Parser[A],pb:Parser[B]):Parser[(A,B)] = for
  {
    a <- pa
    b <- pb
  } yield (a,b)

  def whiteSpaces:Parser[List[Char]] = everythingUntil(!isWhiteSpace(_))

  def whiteSpace1:Parser[List[Char]] = for {
    a <- whiteSpace
    b <- whiteSpaces
  } yield a::b

  // a prefixed by one or more spaces
  def spaceSeq[A](pa: Parser[A]):Parser[A] = for {
  	(a,b) <- seq(whiteSpace1, pa)
  } yield b

  // string prefixed by one or more spaces
  def spaceString(s: String):Parser[String] = spaceSeq(string(s))

  def sep(separator:String):Parser[String] = for {
    _ <- whiteSpaces
    s <- string(separator)
    _ <- whiteSpaces
  } yield s

  def prefixed(prefix:Parser[String])(tail:Parser[String]):Parser[String] = for {
  	(x,xs) <- seq(prefix, optional(tail))
  } yield x + toOption(xs).getOrElse("")

  def digits:Parser[String] = for {
  	s <- many1(digit)
	} yield s.mkString

	def hexChar:Parser[Char] = sat(x => x.isDigit || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

	def hex:Parser[String] = {
		def hexByte:Parser[List[Char]] = for {
			first <- hexChar
			second <- hexChar
		} yield List(first,second)

		for {
			x <- many(hexByte)
		} yield x.flatMap(x => x).mkString
	}

	//def unicodeChar:Parser[String] = anyAttempt(prefix(string("\\x"))())

	// strings like "0x1234"
  def xdigits:Parser[String] = for {
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

  def isPresent[A](a: \/[A,Unit]): Boolean = toOption(a) == None

  def toEither[A,B](a: \/[A,B]): Either[A,B] = a match {
    case -\/(x) => Left(x) 
    case \/-(x) => Right(x)   
  }

  def any[A](parsers: List[Parser[A]]):Parser[A] = parsers.reduceLeft((a,b) => +++(a)(b))
  def anyAttempt[A](parsers: List[Parser[A]]):Parser[A] = parsers.reduceLeft((a,b) => +++(attempt(a))(attempt(b)))
}