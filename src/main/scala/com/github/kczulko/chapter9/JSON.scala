package com.github.kczulko.chapter9

import scala.util.parsing.combinator.RegexParsers

trait JSON

case object JsNull extends JSON
case class JsNumber(n: Double) extends JSON
case class JsString(s: String) extends JSON
case class JsBool(boolean: Boolean) extends JSON
case class JsObject(map: Map[String, JSON]) extends JSON
case class JsArray(array: IndexedSeq[JSON]) extends JSON

trait JsonParser extends RegexParsers {

  private def jsonSeparator: Parser[String] = ""","""
  private def quote: Parser[String] = """""""

  private def jsValue: Parser[JSON] =
    jsNull | jsString | jsBool | jsNumber | jsArray | jsObject

  private def jsNull: Parser[JsNull.type] =
    """null""".r.map(_ => JsNull)
  private def jsString: Parser[JsString] =
    (quote~>"""\w*""".r<~quote).map(JsString)
  private def jsNumber: Parser[JsNumber] =
    """(0|[1-9]\d*)""".r.map(_.toDouble).map(JsNumber)
  private def jsBool: Parser[JsBool] =
    ("true" | "false").map(_.toBoolean).map(JsBool)
  private def jsArray: Parser[JsArray] =
    ("""[""" ~> repsep(jsValue, jsonSeparator) <~ """]""").map(_.toIndexedSeq).map(JsArray)
  def jsObject: Parser[JsObject] =
    "{" ~> repsep((quote~>"""\w*""".r<~quote) ~ ":" ~ jsValue, jsonSeparator) <~ "}" ^^ {
      _.foldRight(JsObject(Map.empty)) { case (key~_~value, JsObject(map)) => JsObject(map + (key -> value)) }
    }
}

object Main extends JsonParser {
  def main(args: Array[String]): Unit = {
    def json =
      """
        |{
        |  "string": "s",
        |  "booleanTrue": true,
        |  "": false,
        |  "jsNull": null,
        |  "array": [
        |     1,
        |     2,
        |     "s",
        |     false,
        |     null,
        |     {}
        |  ],
        |  "jsObj": {
        |   "a": "b"
        |  }
        |}
      """.stripMargin

    val v = parse(jsObject, json)
    println {
      v
    }
  }
}