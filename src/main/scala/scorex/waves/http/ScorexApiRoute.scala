package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import com.wavesplatform.settings.Constants
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions, JsonResponse}
import scorex.network.Coordinator.GetStatus
import scorex.consensus.mining.BlockGeneratorController.GetBlockGenerationStatus
import scorex.app.RunnableApplication
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Deprecated
@Path("/scorex")
@Api(value = "scorex", description = "General commands & information", position = 0)
case class ScorexApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

  val settings = application.settings

  override lazy val route =
    pathPrefix("scorex") {
      scorex ~ status ~ version
    }

  @Deprecated
  @Path("/version")
  @ApiOperation(value = "Version", notes = "get Scorex version", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json Scorex version")
  ))
  def version: Route = {
    path("version") {
      getJsonRoute {
        JsonResponse(Json.obj("version" -> Constants.AgentName), StatusCodes.OK)
      }
    }
  }

  @Deprecated
  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the app", httpMethod = "POST")
  def scorex: Route = path("stop") {
    withAuth {
      postJsonRoute {
        log.info("Request to stop application")
        application.shutdown()
        JsonResponse(Json.obj("stopped" -> true), StatusCodes.OK)
      }
    }
  }

  @Deprecated
  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core", httpMethod = "GET")
  def status: Route = path("status") {
    getJsonRoute {
      def bgf = (application.blockGenerator ? GetBlockGenerationStatus).map(_.toString)
      def hsf = (application.coordinator ? GetStatus).map(_.toString)

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
