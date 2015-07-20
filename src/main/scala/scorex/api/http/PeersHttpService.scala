package scorex.api.http

import java.net.InetSocketAddress

import akka.pattern.ask
import play.api.libs.json.Json
import scorex.Controller
import scorex.block.BlockchainController
import scorex.network.NetworkController
import scorex.network.NetworkController.PeerData
import spray.routing.HttpService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

trait PeersHttpService extends HttpService with CommonApiFunctions {

  lazy val peersRouting =
    pathPrefix("peers") {
      path("") {
        get {
          onComplete {
            (Controller.networkController ? NetworkController.GetPeers).map { peers =>
              Json.obj("peers" -> Json.arr(peers.asInstanceOf[Map[InetSocketAddress, PeerData]]
                .map(_._1.getAddress.toString))).toString()
            }
          } {
            case Success(value) => complete(value)
            case Failure(ex) => failWith(ex)
          }
        }
      } ~ path("height") {                       //todo:fix
        get {
          onComplete {
            (Controller.blockchainController ? BlockchainController.GetMaxChainScore).map { peerHeightsRaw =>
              val peerHeights = peerHeightsRaw.asInstanceOf[Map[InetSocketAddress, Int]]
              Json.arr(peerHeights.map { case (peer, h) =>
                Json.obj("peer" -> peer.getAddress.getHostAddress, "height" -> h)
              }).toString()
            }
          } {
            case Success(value) => complete(value)
            case Failure(ex) => failWith(ex)
          }
        }
      }
    }
}