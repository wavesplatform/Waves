package scorex.lagonaki.api.http

import akka.actor.ActorRefFactory
import akka.pattern.ask
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.lagonaki.server.LagonakiApplication
import scorex.lagonaki.server.settings.Constants
import scorex.lagonaki.network.BlockchainSyncer
import spray.routing.HttpService._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class ScorexApiRoute(application: LagonakiApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
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
              Json.obj("status" -> status.asInstanceOf[String]).toString()
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