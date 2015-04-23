package api.http

import akka.pattern.ask
import controller.Controller
import play.api.libs.json.Json
import scorex.block.BlockchainController
import settings.Constants
import spray.routing.HttpService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ScorexHttpService extends HttpService with CommonApifunctions {
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
            (Controller.blockchainController ? BlockchainController.GetStatus).map { status =>
              Json.obj("status" -> status.asInstanceOf[BlockchainController.Status.Value].toString).toString()
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