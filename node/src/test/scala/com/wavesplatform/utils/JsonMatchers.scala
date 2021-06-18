package com.wavesplatform.utils

import org.scalatest.matchers.{Matcher, MatchResult}
import play.api.libs.json.{Json, JsValue}

trait JsonMatchers {
  def matchJson(value: JsValue): JsonWord = JsonWord(value)
  def matchJson(value: String): JsonWord  = matchJson(Json.parse(value))

  case class JsonWord(value: JsValue) extends Matcher[JsValue] {
    def apply(left: JsValue): MatchResult = {
      MatchResult(
        left == value,
        s"${Json.prettyPrint(left)}\n was not equal to \n${Json.prettyPrint(value)}",
        s"${Json.prettyPrint(left)}\n was equal to \n${Json.prettyPrint(value)}"
      )
    }
  }
}
