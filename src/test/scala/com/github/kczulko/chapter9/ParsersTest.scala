package com.github.kczulko.chapter9

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import Parsers._
import org.scalacheck.Gen

import scala.language.postfixOps
import scala.util.matching.Regex

class ParsersTest extends FlatSpec with Matchers with PropertyChecks {

  "parser" should "hold basic law for char parser" in {
    Parsers.run(char('c'))('c'.toString) shouldEqual Right('c')
  }

  "or" should "hold associativity law when run with alternative parsers" in {
    val abraOrCadabra = "abra" or "cadabra"

    Parsers.run(abraOrCadabra)("abra") shouldEqual Right("abra")
    Parsers.run(abraOrCadabra)("cadabra") shouldEqual Right("cadabra")
  }

  it should "handle either first or second expression" in {
    val parser: Parser[String] ="a" | "b"

    Parsers.run(parser)("b") shouldEqual Right("b")
    Parsers.run(parser)("a") shouldEqual Right("a")
  }

  "listOfN" should "repeat matches while called with listOfN" in {
    Parsers.run(listOfN(3, "ab" | "cad"))("ababcad") shouldEqual Right(List("ab","ab","cad"))
    Parsers.run(listOfN(3, "ab" | "cad"))("cadabab") shouldEqual Right(List("cad","ab","ab"))
    Parsers.run(listOfN(3, "ab" | "cad"))("ababab") shouldEqual Right(List("ab","ab","ab"))
  }

  "string parsers" should "return beginning of the input string" in {
    forAll(Gen.alphaLowerStr) {
      s =>
        Parsers.run("")(s) shouldEqual Right("")
        Parsers.run(s)(s) shouldEqual Right(s)
        if (s.length > 4) {
          val substr = s.take(4)
          val reversedSubst = substr.reverse
          Parsers.run(string(substr))(s) shouldEqual Right(substr)
          if (substr != reversedSubst) {
            Parsers.run(reversedSubst)(s).isLeft shouldEqual true
          }
        }
    }
  }

  "many" should "produce array of proper length" in {
    Parsers.run("a" many)("aaaaa") shouldEqual Right(List.fill(5)("a"))
    Parsers.run("a" many)("aaaaab").isLeft shouldEqual true
    Parsers.run("a" many)("") shouldEqual Right(List.empty)
  }

  "many1" should "fail when input string is empty" in {
    Parsers.run("a" many1)("").isLeft shouldEqual true
  }

  "slice" should "concatenate valid result into single string as long as parser succeds" in {
    val positive = "aaabaa"
    val negative = "aabbcaa"

    val aOrB = ("a" | "b") slice

    Parsers.run(aOrB)(positive) shouldEqual Right(positive)
    Parsers.run(aOrB)(negative).isLeft shouldEqual true
    Parsers.run(aOrB)("") shouldEqual Right("")
  }

  "product" should "allow to join two parsers and run them sequentially" in {
    val parser: Parser[((String, String), String)] = "a" ** "b" ** "c"

    Parsers.run(parser)("abc") shouldEqual Right((("a", "b"), "c"))
//    Parsers.run(""""""" ** "\\d+".r ** """"""")(""""2"""") shouldEqual Right((("\"", "any"), "\""))
  }

  "regex" should "allow to parse with given regex expression" in {
    val parser: Parser[String] = """\w+""".r

    (""""""" ** parser ** """"""").apply(Location(""""dz"""")) shouldEqual Success((("\"", "dz"), "\""), 4)
  }
}
