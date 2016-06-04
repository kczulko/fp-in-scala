package com.kczulko.chapter4

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): MyEither[String, Name] =
    if (name == "" || name == null) MyLeft("Name is empty")
    else MyRight(new Name(name))

  def mkAge(value: Int): MyEither[String, Age] =
    if (value < 0) MyLeft("Age must be greater or equal 0")
    else MyRight(new Age(value))

  def mkPerson(name: String, age: Int): MyEither[String, Person] =
    mkName(name).map2(mkAge(age)){ Person(_, _) }
}
