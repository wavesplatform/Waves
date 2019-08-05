package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.http.{RestAPISettingsHelper, RouteSpec}
import com.wavesplatform.settings.RestAPISettings
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import scala.concurrent.Future

class ApiRouteSpec extends RouteSpec("/test") with RestAPISettingsHelper {
  import akka.http.scaladsl.server.Directives._
  implicit val scheduler: Scheduler = monix.execution.Scheduler.Implicits.global

  class ApiRouteWithSettings(prefix: String, action: => ToResponseMarshallable, val settings: RestAPISettings = restAPISettings)
      extends ApiRoute
      with WithSettings
      with RestAPISettingsHelper {
    override protected val route: Route = path(prefix)(get(complete(action)))
  }

  private val compositeRoute = concat(
    new ApiRouteWithSettings("soe", throw new StackOverflowError()).getRoute,
    new ApiRouteWithSettings("re", throw new RuntimeException()).getRoute,
    new ApiRouteWithSettings("taskSoe", Task(throw new StackOverflowError()).runToFuture).getRoute,
    new ApiRouteWithSettings("taskRe", Task(throw new RuntimeException()).runToFuture).getRoute,
    new ApiRouteWithSettings("coevalSoe", Coeval[ToResponseMarshallable](throw new StackOverflowError()).value()).getRoute,
    new ApiRouteWithSettings("coevalRe", Coeval[ToResponseMarshallable](throw new RuntimeException()).value()).getRoute,
    new ApiRouteWithSettings("futureRe", Future(throw new RuntimeException())).getRoute,
    new ApiRouteWithSettings("taskExecuteAsyncRe", Task(throw new RuntimeException()).executeAsync.runToFuture).getRoute,
    new ApiRouteWithSettings("taskEvalAsyncRe", Task.evalAsync(throw new RuntimeException()).runToFuture).getRoute,
    new ApiRouteWithSettings("taskFromFutureRe", Task.deferFuture(Future(throw new RuntimeException())).runToFuture).getRoute
  )

  // Any of this route cause JVM exit in case of "akka.jvm-exit-on-fatal-error" is turned on
  /*
  private val brokenRoute = concat(
    new ApiRouteWithSettings("future", Future(throw new StackOverflowError())).getRoute,
    new ApiRouteWithSettings("taskExecuteAsync", Task(throw new StackOverflowError()).executeAsync.runToFuture).getRoute,
    new ApiRouteWithSettings("taskEvalAsync", Task.evalAsync(throw new StackOverflowError()).runToFuture).getRoute,
    new ApiRouteWithSettings("taskFromFuture", Task.deferFuture(Future(throw new StackOverflowError())).runToFuture).getRoute
  )
  */

  "StackOverflowError in API should be caught" in {
    Get("/soe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error in API should be caught" in {
    Get("/re") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "StackOverflowError error from Task in API should be caught" in {
    Get("/taskSoe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Task in API should be caught" in {
    Get("/taskRe") ~> compositeRoute ~> check {
      println(responseAs[String])
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "StackOverflowError error from Coeval in API should be caught" in {
    Get("/coevalSoe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Coeval in API should be caught" in {
    Get("/coevalRe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Future in API should be caught" in {
    Get("/futureRe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Task executed asynchronously in API should be caught" in {
    Get("/taskExecuteAsyncRe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Task evaluated asynchronously in API should be caught" in {
    Get("/taskEvalAsyncRe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }

  "NonFatal error from Task deferred from Future in API should be caught" in {
    Get("/taskFromFutureRe") ~> compositeRoute ~> check {
      val response = responseAs[JsObject]
      status shouldBe StatusCodes.InternalServerError
      (response \ "error").as[Int] shouldBe ApiError.Unknown.id
      (response \ "message").as[String] shouldBe ApiError.Unknown.message
    }
  }
}
