package scorex.lagonaki.api.http

import java.net.InetSocketAddress

import akka.actor.ActorRefFactory
import akka.pattern.ask
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.lagonaki.server.LagonakiApplication
import scorex.lagonaki.network.{BlockchainSyncer, NetworkController}
import scorex.lagonaki.network.NetworkController.PeerData
import spray.routing.HttpService
import spray.routing.HttpService._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

@Api(value = "/peers", description = "Get info about peers", position = 1)
case class PeersHttpService(application: LagonakiApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("peers") {
      peers ~ height
    }

  @ApiOperation(value = "Peers list", notes = "Peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response peer list")
  ))
  lazy val peers = path("") {
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
  }


  //TODO ????
  @ApiOperation(value = "Height", notes = "", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  lazy val height = path("height") {
    //todo:fix
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