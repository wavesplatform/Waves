package scorex.api.http

import akka.http.scaladsl.testkit.RouteTest
import org.scalatest.matchers.{MatchResult, Matcher}
import play.api.libs.json._
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._

trait ApiErrorMatchers { this: RouteTest =>
  class ProduceError(error: ApiError) extends Matcher[RouteTestResult] {
    override def apply(left: RouteTestResult): MatchResult = left ~> check {
      if (response.status != error.code) {
        MatchResult(false, "got unexpected status code", "got expected status code")
      } else {
        val responseJson = responseAs[JsObject]
        MatchResult(responseJson == error.json,
          "expected {0}, but instead got {1}",
          "expected not to get {0}, but instead did get it",
          IndexedSeq(error.json, responseJson))
      }
    }
  }

  def produce(error: ApiError) = new ProduceError(error)
}
