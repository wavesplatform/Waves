package scorex.app.api.http

import akka.pattern.ask
import play.api.libs.json.Json
import scorex.app.LagonakiApplication
import scorex.app.settings.Constants
import scorex.network.BlockchainSyncer
import spray.routing.HttpService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ScorexHttpService extends HttpService with CommonApiFunctions {

  val application:LagonakiApplication

  lazy val scorexRouting =
    pathPrefix("scorex") {
      path("stop") {
        get {
          complete {
            Future(application.stopAll())
            Json.obj("stopped" -> true).toString()
          }
        }
      } ~ path("status") {
        get {
          onComplete {
            (application.blockchainSyncer ? BlockchainSyncer.GetStatus).map { status =>
              Json.obj("status" -> status.asInstanceOf[BlockchainSyncer.Status.Value].toString).toString()
            }
          } {
            case Success(value) => complete(value)
            case Failure(ex) => failWith(ex)
          }
        }
      } ~ path("version") {
        get(complete(Json.obj("version" -> Constants.AgentName).toString()))
      }
    }
}