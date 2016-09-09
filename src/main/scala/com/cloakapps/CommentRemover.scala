package com.cloakapps

/**
  * Created by Qiming Li on 9/8/2016.
  */
object CommentRemover {
  def strip(in:List[String]):List[String] = in match {
    case List() => List()
    case x::xs => stripLine(x) :: strip(xs)
  }

  def stripSeq(in:String)(f:Seq[String => String]) = f.foldLeft(in)((x,f) => f(x))

  def stripLine(in:String):String = stripSeq(in.trim())(List(stripSingle, stripMulti, stripOpen, stripClose))

  def stripSingle(in:String):String = {
    val single = "^(.*)//.*$".r
    single findFirstIn in match {
      case Some(single(x)) => x
      case _ => in
    }
  }

  def stripMulti(in:String):String = {
    val Multi = "^(.*?)/\\*.*\\*/(.*)$".r
    in match {
      case Multi(x,y) => x + " " + y
      case _ => in
    }
  }

  def stripOpen(in:String):String = {
    val Open = "^(.*?)/\\*.*$".r
    in match {
      case Open(x) => x
      case _ => in
    }
  }

  def stripClose(in:String):String = {
    val Close = "^.*\\*/(.*)$".r
    in match {
      case Close(x) => x
      case _ => in
    }
  }
}