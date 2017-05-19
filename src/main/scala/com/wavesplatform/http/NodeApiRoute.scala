package com.wavesplatform.http

import javax.ws.rs.Path

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.wavesplatform.Shutdownable
import com.wavesplatform.settings.{Constants, RestAPISettings}
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings, application: Shutdownable, blockGenerator: ActorRef, coordinator: ActorRef)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

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

    complete(StatusCodes.BadRequest)
  }
}
