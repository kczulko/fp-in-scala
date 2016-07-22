package com.kczulko.chapter8

trait Prop {
  def &&(p: Prop): Prop = ???
  def check: Unit
}

trait Gen[+A] {
  def listOf[B](a: Gen[B]): Gen[List[B]]
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
