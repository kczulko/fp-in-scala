package com.github.kczulko.chapter9

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class JsonParserTest extends FlatSpec with Matchers with PropertyChecks {

  import Parsers._

  private def quote: Parser[String] = """""""

  private def jsValue: Parser[JSON] =
    jsNull | jsString | jsBool | jsNumber | jsArray | jsObject

  private def jsNull: Parser[JsNull.type] =
    stringP("""null""") map (_ => JsNull)
  private def jsString: Parser[JsString] =
    (quote ** """\w+""".r ** quote).map {
      case ((_, v), _) => JsString(v)
    }
  private def jsNumber: Parser[JsNumber] =
    """\d+(\.\d+)?""".r.map(_.toDouble).map(JsNumber)
  private def jsBool: Parser[JsBool] =
    ("true" | "false").map(_.toBoolean).map(JsBool)
  private def jsArray: Parser[JsArray] =
    ("""[""" ** manyWithSeparator(jsValue, ",") ** """]""") map {
      case ((_, values), _) => JsArray(values.toIndexedSeq)
    }

  def jsObject: Parser[JsObject] =
    ("{" ** manyWithSeparator(quote ** """\w+""".r ** quote ** ":" ** jsValue, ",") ** "}") map {
        case ((_, keyValueObjects), _) =>
          JsObject(
            keyValueObjects.map {
              case ((((_, jsonKey), _), _), jsonValue) => jsonKey -> jsonValue
            }.toMap
          )
      }

  it should "parse empty object" in {
    val json = """{}"""
    Parsers.run(jsObject)(json) shouldEqual Right(JsObject(Map.empty))
  }

  it should "parse simple object with string value" in {
    val json = """{"key":"value"}"""
    Parsers.run(jsObject)(json) shouldEqual Right(JsObject(Map("key" -> JsString("value"))))
  }

  it should "parse simple object with number value" in {
    val json = """{"key":2}"""
    Parsers.run(jsObject)(json) shouldEqual Right(JsObject(Map("key" -> JsNumber(2.0))))
  }

  it should "parse simple object with null value" in {
    val json = """{"key":null}"""
    Parsers.run(jsObject)(json) shouldEqual Right(JsObject(Map("key" -> JsNull)))
  }

  it should "parse simple object with boolean value" in {
    val jsonTrue = """{"key":true}"""
    val jsonFalse = """{"key":false}"""
    Parsers.run(jsObject)(jsonTrue) shouldEqual Right(JsObject(Map("key" -> JsBool(true))))
    Parsers.run(jsObject)(jsonFalse) shouldEqual Right(JsObject(Map("key" -> JsBool(false))))
  }

  it should "parser json object with more that one item" in {
    val json = """{"key":3.1,"isNull":true,"some":null}"""

    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(Map("key" -> JsNumber(3.1), "isNull" -> JsBool(true), "some" -> JsNull))
    )
  }

  it should "parse simple object with empty array item" in {
    val json = """{"array":[]}"""
    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(Map("array" -> JsArray(Vector.empty)))
    )
  }

  it should "parse simple object with array item" in {
    val json = """{"array":["b",null]}"""
    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(Map("array" -> JsArray(Vector(JsNull, JsString("b")))))
    )
  }

  it should "parse simple object with array item containg digits" in {
    val json = """{"array":[4]}"""
    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(Map("array" -> JsArray(Vector(JsNumber(4)))))
    )
  }

  it should "parse chained JsObjects" in {
    val json ="""{"first":{"second":null},"third":{"fourth":[]}}"""
    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(
        Map(
          "first" -> JsObject(Map("second" -> JsNull)),
          "third" -> JsObject(Map("fourth" -> JsArray(Vector.empty)))
        )
      )
    )
  }

  it should "be able to parse complex json" in {
    val json =
      """{"string":"s","booleanTrue":true,"q":false,"jsNull":null,"array":[1,2,"s",false,null,{}],"jsObj":{"a":"b"}}"""

    Parsers.run(jsObject)(json) shouldEqual Right(
      JsObject(
        Map(
          "string" -> JsString("s"),
          "booleanTrue" -> JsBool(true),
          "q" -> JsBool(false),
          "jsNull" -> JsNull,
          "array" -> JsArray(
            Vector(
              JsObject(Map.empty), JsNull, JsBool(false), JsString("s"), JsNumber(2), JsNumber(1)
            )
          ),
          "jsObj" -> JsObject(
            Map(
              "a" -> JsString("b")
            )
          )
        )
      )
    )
  }
}
