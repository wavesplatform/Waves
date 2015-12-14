package scorex.lagonaki.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.pattern.ask
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.lagonaki.server.LagonakiApplication
import scorex.network.{ConnectedPeer, NetworkController}
import spray.http.MediaTypes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersHttpService(application: LagonakiApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("peers") {
      peers // todo: fix ~ score
    }

  @Path("/")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def peers = path("") {
    get {
      respondWithMediaType(`application/json`) {
        onComplete {
          (application.networkController ? NetworkController.GetPeers).map { peers =>
            Json.obj("peers" -> Json.arr(peers.asInstanceOf[Seq[ConnectedPeer]]
              .map(_.address.toString))).toString()
          }
        } {
          case Success(value) => complete(value)
          case Failure(ex) => failWith(ex)
        }
      }
    }
  }

  /*
  todo: fix

  @Path("/score")
  @ApiOperation(value = "Score", notes = "Node with a maximum blockchain score", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error"),
    new ApiResponse(code = 500, message = "Internal error")
  ))
  def score = path("score") {
    //todo:fix
    get {
      respondWithMediaType(`application/json`) {
        onComplete {
          (application.blockchainSyncer ? BlockchainGenerator.GetMaxChainScore).map { peerHeightsRaw =>
            val peerHeights = peerHeightsRaw.asInstanceOf[Map[InetSocketAddress, Int]]
            Json.arr(peerHeights.map { case (peer, h) =>
              Json.obj("peer" -> peer.getAddress.getHostAddress, "score" -> h)
            }).toString()
          }
        } {
          case Success(value) => complete(value)
          case Failure(ex) => failWith(ex)
        }
      }
    }
  } */

}