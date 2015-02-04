package api

import controller.Controller
import play.api.libs.json.JsArray
import spray.routing.HttpService


trait BlocksHttpService extends HttpService with CommonApifunctions {

  lazy val route =
    path("/") {
      get {
        complete(walletNotExists().getOrElse(JsArray(Controller.getLastBlocks().map(_._2.toJson()))).toString())
      }
    }

}
