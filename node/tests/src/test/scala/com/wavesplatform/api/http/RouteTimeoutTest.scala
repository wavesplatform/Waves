package com.wavesplatform.api.http

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.wavesplatform.test.PropSpec
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean

import scala.concurrent.duration.DurationInt

class RouteTimeoutTest extends PropSpec with ApiMarshallers with ScalatestRouteTest {

  implicit val sc: Scheduler                   = Scheduler.global
  implicit val ess: JsonEntityStreamingSupport = EntityStreamingSupport.json()

  property("return timeout response and cancel inner task") {
    val bool = AtomicBoolean(false)

    val timeout      = 1.second
    val task         = Task.sleep(timeout + 1.second) *> Task(bool.set(true)) *> Task(Seq(true))
    val routeTimeout = new RouteTimeout(timeout)

    val routes = Seq(
      routeTimeout.execute(task)((t, _) => t.runSyncUnsafe()),
      routeTimeout.executeToFuture(task),
      routeTimeout.executeStreamed(task)(identity)
    )

    implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(timeout + 1.second)

    routes.foreach { route =>
      Get() ~> route ~> check {
        response.status shouldBe StatusCodes.ServiceUnavailable
        responseAs[String] should include("The server was not able to produce a timely response")
        bool.get() shouldBe false
      }
    }
  }
}
