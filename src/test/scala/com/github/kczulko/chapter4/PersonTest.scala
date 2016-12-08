package com.kczulko.chapter4

import com.github.kczulko.chapter4.Person
import com.github.kczulko.chapter4.Person.mkPerson
import org.scalatest.{FlatSpec, Matchers}

class PersonTest extends FlatSpec with Matchers {

  behavior of "PersonTest"

  it should "return list of errors when invalid params passed" in {
    mkPerson("", -2) shouldEqual AccLeft(List("Age must be greater or equal 0", "Name is empty"))
    mkPerson("", 12) shouldEqual AccLeft(List("Name is empty"))
    mkPerson("AnyName", -2) shouldEqual AccLeft(List("Age must be greater or equal 0"))
  }

  it should "return valid Person object for valid params" in {
    var either = mkPerson("AnyName", 2)
    either.isInstanceOf[AccRight[Person]] shouldEqual true
    either.asInstanceOf[AccRight[Person]].value.age.value shouldEqual 2
    either.asInstanceOf[AccRight[Person]].value.name.value shouldEqual "AnyName"
  }

}
