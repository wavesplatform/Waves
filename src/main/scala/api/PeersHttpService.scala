package api

import controller.Controller
import play.api.libs.json.Json
import spray.routing.HttpService

trait PeersHttpService extends HttpService with CommonApifunctions {
  lazy val peersRouting =
    pathPrefix("peers") {
      path("") {
        get {
          complete(
            Json.obj("peers" -> Json.arr(Controller.activePeers().map(_.address.getHostAddress).toSeq)).toString()
          )
        }
      } ~ path("height") {
        get {
          complete(
            Json.arr(Controller.peerHeights.map { case (peer, h) =>
              Json.obj("peer" -> peer.address.getHostAddress, "height" -> h)
            }).toString()
          )
        }
      }
    }
}