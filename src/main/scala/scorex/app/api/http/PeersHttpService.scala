package scorex.app.api.http

import java.net.InetSocketAddress

import akka.pattern.ask
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.app.LagonakiApplication
import scorex.network.{BlockchainSyncer, NetworkController}
import scorex.network.NetworkController.PeerData
import spray.routing.HttpService._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

case class PeersHttpService(application:LagonakiApplication) extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("peers") {
      path("") {
        get {
          onComplete {
            (application.networkController ? NetworkController.GetPeers).map { peers =>
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
            (application.blockchainSyncer ? BlockchainSyncer.GetMaxChainScore).map { peerHeightsRaw =>
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