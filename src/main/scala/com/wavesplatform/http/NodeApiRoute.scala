package com.wavesplatform.http

import java.time.Instant
import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.Shutdownable
import com.wavesplatform.network.lastObserved
import com.wavesplatform.settings.{Constants, RestAPISettings}
import com.wavesplatform.state2.StateWriter
import io.swagger.annotations._
import monix.eval.Coeval
import monix.execution.Scheduler.Implicits.global
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.transaction.{BlockchainUpdater, LastBlockInfo}
import scorex.utils.ScorexLogging

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings, blockchainUpdater: BlockchainUpdater, state: StateWriter, application: Shutdownable)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

  private val lastHeight: Coeval[Option[LastBlockInfo]] = lastObserved(blockchainUpdater.lastBlockInfo)
  private val lastState: Coeval[Option[StateWriter.Status]] = lastObserved(state.status)

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
    val (bcHeight, bcTime) = lastHeight().map { case LastBlockInfo(_, h, _, _, t) => (h, t) }.getOrElse((0, 0L))
    val (stHeight, stTime) = lastState().map { case StateWriter.Status(h, t) => (h, t) }.getOrElse((0, 0L))
    val lastUpdated = bcTime max stTime
    complete(Json.obj(
      "blockchainHeight" -> bcHeight,
      "stateHeight" -> stHeight,
      "updatedTimestamp" -> lastUpdated,
      "updatedDate" -> Instant.ofEpochMilli(lastUpdated).toString
    ))
  }
}
