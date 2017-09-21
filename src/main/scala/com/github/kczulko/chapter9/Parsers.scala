package com.github.kczulko.chapter9

import scala.util.matching.Regex

object Parsers {
  type Parser[+A] = Location => Result[A]
  sealed trait Result[+A] {
    def mapError(f: ParserError => ParserError) = this match {
      case Failure(pe, ic) => Failure(f(pe), ic)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(pe, true) => Failure(pe, isCommited = false)
      case _ => this
    }
  }
  case class Success[A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParserError, isCommited: Boolean) extends Result[Nothing]

  case class ParserError(stack: List[(Location, String)]) {
    def push(location: Location, msg: String): ParserError = copy(stack = (location, msg) :: stack)
    def label[A](s: String): ParserError = ParserError(latestLocation.map((_,s)).toList)
    def latest = stack.lastOption
    def latestLocation = latest map (_._1)
  }
  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.substring(0,offset+1)
    lazy val col = input.substring(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset+1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParserError = ParserError(List((this, msg)))
  }

  def run[A](p: Parser[A])(input: String): Either[ParserError, A] = {
    val location = Location(input)
    p(location) match {
      case Success(a, _) => Right(a)
      case Failure(pe, _) => Left(pe)
    }
  }

  // primitives
  def stringP(s: String): Parser[String] = location =>
    if (location.input.startsWith(s)) {
      Success(s, s.length)
    } else {
      Failure(location.toError(s"Expected '$s' but found '${location.input}'"), isCommited = true)
    }

  def slice[A](p: Parser[A]): Parser[String] = ???
  def many[A](p: Parser[A]): Parser[List[A]] = ???
  def product[A,B](a: Parser[A], b: => Parser[B]): Parser[(A,B)] = ???
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = l => p(l) match {
    case Success(a, c) => f(a)(l.copy(offset = c))
    case f @ Failure(_, _) => f
  }
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = l => s1(l) match {
    case Failure(_, false) => s2(l)
    case any => any
  }
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = ???
  implicit def string(s: String): Parser[String] = stringP(s)

  implicit def regex(r: Regex): Parser[String] = l =>
    r.findFirstMatchIn(l.input)
      .map(m => Success(l.input.substring(m.start, m.end), m.end))
      .getOrElse(Failure(l.toError(s"Cannot find $r within ${l.input}"), true))


  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = stringP("").map(_ => a)
  def map2[A,B,C](a: Parser[A], b: => Parser[B])(f: (A,B) => C): Parser[C] =
    ParserOps(product(a,b)).map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] = map(product(p, many(p))) { case (a, list) => a :: list }
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case i if i <= 0 => succeed(List[A]())
    case _ => map2(p, listOfN(n -1, p))(_ :: _)
  }
  def manyInTermsOfOrMap2andSucceed[A](p: Parser[A]): Parser[List[A]] =
    or(
      map2(p, manyInTermsOfOrMap2andSucceed(p))(_ :: _),
      succeed(List[A]())
    )
  def product2[A,B](a: Parser[A], b: => Parser[B]): Parser[(A,B)] = map2(a, b)((_,_))
  def map2_2[A,B,C](a: Parser[A], b: Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(a)(aa => flatMap(b)(bb => succeed(f(aa,bb))))
  def map_2[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def label[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.label(msg))
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.push(l, msg))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = Parsers.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = Parsers.or(p, p2)
    def many: Parser[List[A]] = Parsers.many(p)
    def slice: Parser[String] = Parsers.slice(p)
    def map[B](f: A => B): Parser[B] = Parsers.map(p)(f)
  }

  def errorLocation(e: ParserError): Location = ???
  def errorMessage(e: ParserError): String = ???
}
