package com.github.kczulko.chapter9

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Parsers._
import org.scalacheck.Gen

class ParsersTest extends FlatSpec with Matchers with PropertyChecks {

  ignore /*"parser"*/ should "hold basic law for char parser" in {
    Parsers.run(char('c'))('c'.toString) shouldEqual Right('c')
  }

  ignore should "hold associativity law when run with alternative parsers" in {
    Parsers.run(or(string("abra"), string("cadabra")))("abra") shouldEqual Right("abra")
    Parsers.run(or(string("abra"), string("cadabra")))("cadabra") shouldEqual Right("cadabra")
  }

  ignore should "repeat matches while called with listOfN" in {
    Parsers.run(listOfN(3, "ab" | "cad"))("ababcad") shouldEqual Right("ababcad")
    Parsers.run(listOfN(3, "ab" | "cad"))("cadabab") shouldEqual Right("cadabab")
    Parsers.run(listOfN(3, "ab" | "cad"))("ababab") shouldEqual Right("ababab")
  }

  "string parsers" should "return beginning of the input string" in {
    forAll(Gen.alphaLowerStr) {
      s =>
        Parsers.run(string(""))(s) shouldEqual Right("")
        Parsers.run(string(s))(s) shouldEqual Right(s)
        if (s.length > 4) {
          val substr = s.take(4)
          val reversedSubst = substr.reverse
          Parsers.run(string(substr))(s) shouldEqual Right(substr)
          if (substr != reversedSubst) {
            Parsers.run(string(reversedSubst))(s).isLeft shouldEqual true
          }
        }
    }
  }
}
