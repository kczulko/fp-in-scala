package com.github.kczulko.chapter9

trait Parsers[ParserError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???

  def char(c: Char): Parser[Char] = ???
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  implicit def string(s: String): Parser[String] = ???
  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many[A](p: Parser[A]): Parser[List[A]] = ???

    def map[A,B](p: Parser[A])(f: A => B): Parser[B] = ???
  }
}
