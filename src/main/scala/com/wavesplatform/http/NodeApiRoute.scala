package com.wavesplatform.http

import java.time.Instant
import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.Shutdownable
import com.wavesplatform.settings.{Constants, RestAPISettings}
import com.wavesplatform.utils.HeightInfo
import io.swagger.annotations._
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.utils.ScorexLogging

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings, heights: Coeval[(HeightInfo, HeightInfo)], application: Shutdownable)
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
    val ((bcHeight, bcTime), (stHeight, stTime)) = heights()
    val lastUpdated = bcTime max stTime
    complete(Json.obj(
      "blockchainHeight" -> bcHeight,
      "stateHeight" -> stHeight,
      "updatedTimestamp" -> lastUpdated,
      "updatedDate" -> Instant.ofEpochMilli(lastUpdated).toString
    ))
  }
}
