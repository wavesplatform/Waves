package com.wavesplatform.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions, JsonResponse}
import scorex.app.RunnableApplication
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.BlockchainSynchronizer
import scorex.utils.ScorexLogging
import com.wavesplatform.settings.Constants

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

  val settings = application.settings
  override lazy val route =
    pathPrefix("node") {
      stop ~ status ~ version
    }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "Get Waves node version", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json Waves node version")
  ))
  def version: Route = {
    path("version") {
      getJsonRoute {
        JsonResponse(Json.obj("version" -> Constants.AgentName), StatusCodes.OK)
      }
    }
  }

  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the node", httpMethod = "POST")
  def stop: Route = path("stop") {
    withAuth {
      postJsonRoute {
        log.info("Request to stop application")
        Future(application.stopAll())
        JsonResponse(Json.obj("stopped" -> true), StatusCodes.OK)
      }
    }
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core(Offline/Syncing/Generating)", httpMethod = "GET")
  def status: Route = path("status") {
    getJsonRoute {
      def bgf = (application.blockGenerator ? GetStatus).map(_.toString)
      def hsf = (application.coordinator ? BlockchainSynchronizer.GetStatus).mapTo[BlockchainSynchronizer.Status].map(_.name)

      Future.sequence(Seq(bgf, hsf)).map { statusesSeq =>
        val json = Json.obj(
          "blockGeneratorStatus" -> statusesSeq.head,
          "historySynchronizationStatus" -> statusesSeq.tail.head
        )

        JsonResponse(json, StatusCodes.OK)
      }
    }
  }
}
