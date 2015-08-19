package scorex.api.http

import akka.pattern.ask
import play.api.libs.json.Json
import scorex.Controller
import scorex.block.BlockchainSyncer$
import scorex.settings.Constants
import spray.routing.HttpService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ScorexHttpService extends HttpService with CommonApiFunctions {
  lazy val scorexRouting =
    pathPrefix("scorex") {
      path("stop") {
        get {
          complete {
            Future(Controller.stopAll())
            Json.obj("stopped" -> true).toString()
          }
        }
      } ~ path("status") {
        get {
          onComplete {
            (Controller.blockchainController ? BlockchainSyncer.GetStatus).map { status =>
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