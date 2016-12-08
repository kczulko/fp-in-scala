package com.github.kczulko.chapter4

import com.kczulko.chapter4.{AccEither, AccLeft, AccRight}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): AccEither[List[String], Name] =
    if (name == "" || name == null) AccLeft(List("Name is empty"))
    else AccRight(new Name(name))

  def mkAge(value: Int): AccEither[List[String], Age] =
    if (value < 0) AccLeft(List("Age must be greater or equal 0"))
    else AccRight(new Age(value))

  def mkPerson(name: String, age: Int): AccEither[List[String], Person] =
    mkName(name).map2(mkAge(age)){ Person(_, _) }
}
