package com.wavesplatform.http

import javax.ws.rs.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import com.wavesplatform.settings.Constants
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.app.RunnableApplication
import scorex.consensus.mining.BlockGeneratorController.GetBlockGenerationStatus
import scorex.network.Coordinator.GetStatus
import scorex.utils.ScorexLogging

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(application: RunnableApplication) extends ApiRoute with CommonApiFunctions with ScorexLogging {

  val settings = application.settings.restAPISettings
  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "Get Waves node version", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json Waves node version")
  ))
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the node", httpMethod = "POST")
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core", httpMethod = "GET")
  def status: Route = (get & path("status")) {
    implicit val timeout = Timeout(5.seconds)

    complete(for {
      bgf <- application.blockGenerator ? GetBlockGenerationStatus
      hsf <- application.coordinator ? GetStatus
    } yield Json.obj(
      "blockGeneratorStatus" -> bgf.toString,
      "historySynchronizationStatus" -> hsf.toString))
  }
}
