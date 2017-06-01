package com.wavesplatform.http

import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.testkit.TestProbe
import com.wavesplatform.Shutdownable
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.Constants
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http.ApiKeyNotValid
import scorex.consensus.mining.{BlockGeneratorController => BGC}
import scorex.network.Coordinator

import scala.concurrent.duration._

class NodeRouteSpec extends RouteSpec("/node") with RestAPISettingsHelper with MockFactory with PropertyChecks {
  import NodeRouteSpec._

  private val blockGenerator = TestProbe()
  private val coordinator = TestProbe()
  private val application = mock[Shutdownable]

  private val route = NodeApiRoute(restAPISettings, application, blockGenerator.ref, coordinator.ref).route

  private val blockGeneratorStatusGen = Gen.oneOf(BGC.Generating, BGC.Idle, BGC.Suspended)
  private val coordinatorStatusGen = Gen.oneOf(Coordinator.CIdle, Coordinator.CSyncing)

  implicit val defaultTimeout = RouteTestTimeout(10.seconds)

  routePath("/status") - {
    "both modules respond" in {
      val statusGen = for {
        bgs <- blockGeneratorStatusGen
        cs <- coordinatorStatusGen
      } yield (bgs, cs)

      forAll(statusGen) { case (bgs, cs) =>
        val result = Get(routePath("/status")) ~> route ~> runRoute
        blockGenerator.expectMsg(BGC.GetStatus)
        blockGenerator.reply(bgs.name)

        coordinator.expectMsg(Coordinator.GetStatus)
        coordinator.reply(cs.name)

        check {
          val resp = responseAs[StatusResp]
          resp.blockGeneratorStatus shouldEqual bgs.name
          resp.historySynchronizationStatus shouldEqual cs.name
        }(result)
      }
    }

    "one of the modules is not responding" in {
      val statusGen = for {
        bgs <- Gen.option(blockGeneratorStatusGen)
        cs <- Gen.option(coordinatorStatusGen)
      } yield (bgs, cs)

      forAll(statusGen) { case (maybeBGS, maybeCS) =>
        val result = Get(routePath("/status")) ~> route ~> runRoute
        blockGenerator.expectMsg(BGC.GetStatus)
        blockGenerator.reply(if (maybeBGS.isDefined) maybeBGS.get.name else "failure")

        coordinator.expectMsg(Coordinator.GetStatus)
        coordinator.reply(if (maybeCS.isDefined) maybeCS.get.name else "failure")

        check {
          val resp = responseAs[StatusResp]
          resp.blockGeneratorStatus shouldEqual (if (maybeBGS.isDefined) maybeBGS.get.name else "failure")
          resp.historySynchronizationStatus shouldEqual (if (maybeCS.isDefined) maybeCS.get.name else "failure")
        }(result)
      }
    }
  }

  routePath("/version") in {
    Get(routePath("/version")) ~> route ~> check {
      (responseAs[JsObject] \ "version").as[String] shouldEqual Constants.AgentName
    }
  }

  routePath("/stop") in {
    Post(routePath("/stop")) ~> route should produce(ApiKeyNotValid)

    (application.shutdown _).expects().once()
    Post(routePath("/stop")) ~> api_key(apiKey) ~> route ~> check {
      (responseAs[JsObject] \ "stopped").as[Boolean] shouldEqual true
    }
  }
}

object NodeRouteSpec {

  case class StatusResp(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[StatusResp] = Json.format
}
