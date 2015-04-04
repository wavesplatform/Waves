package api

import java.net.InetSocketAddress
import controller.Controller
import play.api.libs.json.Json
import scorex.block.BlockchainController
import scorex.network.NetworkController
import spray.routing.HttpService
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

trait PeersHttpService extends HttpService with CommonApifunctions {

  lazy val peersRouting =
    pathPrefix("peers") {
      path("") {
        get {
          onComplete {
            (Controller.networkController ? NetworkController.GetPeers).map {peers =>
              Json.obj("peers" -> Json.arr(peers.asInstanceOf[Set[InetSocketAddress]].map(_.getHostName))).toString()
            }
          } {
            case Success(value) => complete(value)
            case Failure(ex)    => failWith(ex)
          }
        }
      } ~ path("height") {
        get {
          onComplete {
            (Controller.blockchainController ? BlockchainController.GetMaxChainScore).map{peerHeightsRaw =>
              val peerHeights = peerHeightsRaw.asInstanceOf[Map[InetSocketAddress, Int]]
              Json.arr(peerHeights.map { case (peer, h) =>
                Json.obj("peer" -> peer.getAddress.getHostAddress, "height" -> h)
              }).toString()
            }
          } {
            case Success(value) => complete(value)
            case Failure(ex)    => failWith(ex)
          }
        }
      }
    }
}