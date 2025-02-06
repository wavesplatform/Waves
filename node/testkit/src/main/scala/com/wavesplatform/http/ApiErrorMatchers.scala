package com.wavesplatform.http

import akka.http.scaladsl.testkit.RouteTest
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiMarshallers.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import play.api.libs.json.*

trait ApiErrorMatchers extends Matchers { this: RouteTest =>
  class ProduceError(error: ApiError, matchMsg: Boolean) extends Matcher[RouteTestResult] {
    override def apply(left: RouteTestResult): MatchResult = left ~> check {
      if (response.status != error.code) {
        MatchResult(
          false,
          "got {0} while expecting {1}, response was {2}",
          "got expected status code {0}",
          IndexedSeq(response.status, error.code, response.entity)
        )
      } else {
        val responseJson     = responseAs[JsObject]
        val actualResponse   = responseJson - "trace" - "message"
        val actualMessage    = (responseJson \ "message").as[String]
        val expectedResponse = error.json - "message"
        val expectedMessage  = error.message
        MatchResult(
          actualResponse == expectedResponse && (if (matchMsg) actualMessage.matches(s".*$expectedMessage.*") else actualMessage == expectedMessage),
          "expected {0}, but instead got {1}",
          "expected not to get {0}, but instead did get it",
          IndexedSeq(error.json, responseJson)
        )
      }
    }
  }

  def produce(error: ApiError, matchMsg: Boolean = false): ProduceError = new ProduceError(error, matchMsg)
}
