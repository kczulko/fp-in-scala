package com.github.kczulko.chapter9

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???

  // primitives
  def stringP(s: String): Parser[String] = ???
  def slice[A](p: Parser[A]): Parser[String] = ???
  def many[A](p: Parser[A]): Parser[List[A]] = ???
  def product[A,B](a: Parser[A], b: => Parser[B]): Parser[(A,B)] = ???
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = ???
  implicit def string(s: String): Parser[String] = ???
  implicit def regex(r: Regex): Parser[String] = ???

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

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many[A]: Parser[List[A]] = ???
    def slice: Parser[String] = ???
    def map[B](f: A => B): Parser[B] = ???
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.substring(0,offset+1)
    lazy val col = input.substring(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset+1
      case lineStart => offset - lineStart
    }
  }

  def errorLocation(e: ParserError): Location = ???
  def errorMessage(e: ParserError): String = ???

  case class ParserError(stack: List[(Location, String)])
}
