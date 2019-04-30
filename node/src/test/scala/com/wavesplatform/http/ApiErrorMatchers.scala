package com.wavesplatform.http

import akka.http.scaladsl.testkit.RouteTest
import org.scalatest.matchers.{MatchResult, Matcher}
import play.api.libs.json._
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.api.http.ApiError

trait ApiErrorMatchers { this: RouteTest =>
  class ProduceError(error: ApiError) extends Matcher[RouteTestResult] {
    override def apply(left: RouteTestResult): MatchResult = left ~> check {
      if (response.status != error.code) {
        MatchResult(false,
                    "got {0} while expecting {1}, response was {2}",
                    "got expected status code {0}",
                    IndexedSeq(response.status, error.code, response.entity))
      } else {
        val responseJson = responseAs[JsObject]
        MatchResult(responseJson - "trace" == error.json,
                    "expected {0}, but instead got {1}",
                    "expected not to get {0}, but instead did get it",
                    IndexedSeq(error.json, responseJson))
      }
    }
  }

  def produce(error: ApiError) = new ProduceError(error)
}
