package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.pattern.ask
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.app.Application
import scorex.consensus.mining.BlockGeneratorController._
import scorex.waves.settings.Constants
import scorex.network.HistorySynchronizer
import spray.routing.Route

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Api(value = "scorex", description = "General commands & information", position = 0)
case class ScorexApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("scorex") {
      scorex ~ status ~ version
    }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "get Scorex version", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json Scorex version")
  ))
  def version: Route = {
    path("version") {
      jsonRoute {
        Json.obj("version" -> Constants.AgentName).toString()
      }
    }
  }

  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the app", httpMethod = "POST")
  def scorex: Route = path("stop") {
    jsonRoute({
      Future(application.stopAll())
      Json.obj("stopped" -> true).toString()
    }, post)
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core(Offline/Syncing/Generating)", httpMethod = "GET")
  def status: Route = path("status") {
    jsonRoute {
      def bgf = (application.blockGenerator ? GetStatus).map(_.toString)
      def hsf = (application.historySynchronizer ? HistorySynchronizer.GetStatus).map(_.toString)

      Future.sequence(Seq(bgf, hsf)).map { case statusesSeq =>
        Json.obj(
          "block_generator_status" -> statusesSeq.head,
          "history_synchronization_status" -> statusesSeq.tail.head
        ).toString()
      }
    }
  }
}
