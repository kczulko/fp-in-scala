package com.github.kczulko.chapter11

import com.github.kczulko.chapter11.Functor._
import org.scalatest.{FlatSpec, Matchers}

class FunctorTest extends FlatSpec with Matchers with FunctorLaw {

  "Option functor" should behave like functorOf(optionFunctor)(Some(2), None, Some(6))

  "List functor" should behave like functorOf(listFunctor)(List(), List(1243), List(1,2,3,4))
}
