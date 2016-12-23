package com.github.kczulko.chapter12

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: List[E] = List()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]
