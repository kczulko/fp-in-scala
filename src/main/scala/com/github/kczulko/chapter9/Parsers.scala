package com.github.kczulko.chapter9

trait Parsers[ParserError, Parser[+_]] {
  def run[A](p: Parser[A])(intpu: String): Either[ParserError, A]
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
  def or[A](s1: A, s2: A): Parser[A]
}
