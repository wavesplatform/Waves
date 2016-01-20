package scorex.lagonaki.api.http

import javax.ws.rs.Path
import akka.actor.ActorRefFactory
import akka.pattern.ask
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.lagonaki.server.LagonakiApplication
import scorex.lagonaki.server.settings.Constants
import scorex.network.{HistorySynchronizer, BlockGenerator}
import spray.routing.Route
import scala.concurrent.Future
import scala.util.{Failure, Success}

import spray.http.MediaTypes._
import com.wordnik.swagger.annotations._
import scala.concurrent.ExecutionContext.Implicits.global

@Api(value = "scorex", description = "General commands & information", position = 0)
case class ScorexApiRoute(application: LagonakiApplication)(implicit val context: ActorRefFactory)
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
    get {
      respondWithMediaType(`application/json`) {
        onComplete {
          def bgf = (application.blockGenerator ? BlockGenerator.GetStatus).map(_.toString)
          def hsf = (application.historySynchronizer ? HistorySynchronizer.GetStatus).map(_.toString)

          Future.sequence(Seq(bgf, hsf)).map { case statusesSeq=>
            Json.obj(
              "block generator status" -> statusesSeq.head,
              "history synchronization status" -> statusesSeq.tail.head
            ).toString()
          }
        } {
          case Success(value) => complete(value)
          case Failure(ex) => failWith(ex)
        }
      }
    }
  }
}
