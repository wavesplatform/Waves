package api

import controller.Controller
import play.api.libs.json.Json
import spray.routing.HttpService
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ScorexHttpService extends HttpService with CommonApifunctions {
  lazy val scorexRouting =
    pathPrefix("scorex") {
      path("stop") {
        get {
          complete{
            Future(Controller.stopAll())
            Json.obj("stopped" -> true).toString()
          }
        }
      } ~ path("status") {
        get (complete(Json.obj("status" -> Controller.getStatus).toString()))
      } ~ path("isuptodate") {
        get(complete(Json.obj("status" -> Controller.isUpToDate()).toString()))
      } ~ path("version") {
        //todo: pull version string out of here
        get (complete(Json.obj("version" -> "Lagonaki v. 0.9").toString()))
      }
    }
}