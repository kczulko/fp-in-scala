package com.github.kczulko.chapter9

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ParsersTest extends FlatSpec with Matchers with PropertyChecks {

  type F[+_]

  ignore /*"parser"*/ should "hold basic law for char parser" in {
    val parsers = new Parsers[F] {}

    import parsers._

    parsers.run(char('c'))('c'.toString) shouldEqual Right('c')
  }

  ignore should "hold associativity law when run with alternative parsers" in {
    val parsers = new Parsers[F] {}

    import parsers._

    parsers.run(or(string("abra"), string("cadabra")))("abra") shouldEqual Right("abra")
    parsers.run(or(string("abra"), string("cadabra")))("cadabra") shouldEqual Right("cadabra")
  }

  ignore should "repeat matches while called with listOfN" in {
    val parsers = new Parsers[F] {}

    import parsers._

    parsers.run(listOfN(3, "ab" | "cad"))("ababcad") shouldEqual Right("ababcad")
    parsers.run(listOfN(3, "ab" | "cad"))("cadabab") shouldEqual Right("cadabab")
    parsers.run(listOfN(3, "ab" | "cad"))("ababab") shouldEqual Right("ababab")
  }
}
