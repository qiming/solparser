package com.cloakapps

/**
  * Created by Qiming Li on 9/8/2016.
  */
object CommentRemover {
  def strip(in:List[String]):List[String] = strip(0, in)

  def strip(n:Int, in:List[String]):List[String] = in match {
    case List() => List()
    case x::xs => stripLine(n, x) match {
      case (i, y) => y :: strip(i, xs)
    }
  }

  def stripSeq(n:Int, in:String)(f:Seq[(Int, String) => (Int, String)]) = f.foldLeft((n,in))((t,f) => f(t._1,t._2))

  def stripLine(n:Int, in:String):(Int,String) = stripSeq(n, in.trim())(List(stripClose, stripSingle, stripMulti, stripOpen))

  def stripSingle(n:Int, in:String):(Int, String) = {
    val single = """^(.*?)//.*$""".r
    //println(s"single: n = $n")

    if (n > 0) (n, "")
    else in match {
      case single(x) => 
        //println(s"matched single: $in -> $x"); 
        (n, x)
      case _ => (n, in)
    }
  }

  def stripMulti(n:Int, in:String):(Int, String) = {
    val multi = """^(.*?)/\*.*\*/(.*)$""".r
    //println(s"multi: n = $n")

    if (n > 0) (n, "")
    else in match {
      case multi(x,y) => 
        //println(s"matched multi: $in -> $x $y"); 
        (n, x + " " + y)
      case _ => (n, in)
    }
  }

  def stripOpen(n:Int, in:String):(Int, String) = {
    val open = """^(.*?)/\*.*$""".r
    //println(s"open n = $n")

    if (n > 0) (n, "")
    else in match {
      case open(x) => 
        //println(s"matched open: $in -> $x"); 
        (n+1, x)
      case _ => (n, in)
    }
  }

  def stripClose(n:Int, in:String):(Int, String) = {
    val close = """^.*\*/(.*?)$""".r
    //println(s"close n = $n")

    if (n <= 0) (n, in)
    else in match {
      case close(x) => //println(s"matched close: $in -> $x"); 
        (n-1, x)
      case _ => (n, in)
    }
  }
}