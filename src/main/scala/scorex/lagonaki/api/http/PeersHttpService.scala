package scorex.lagonaki.api.http

import java.net.InetSocketAddress
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.pattern.ask
import com.wordnik.swagger.annotations._
import play.api.libs.json.{JsString, Json}
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.app.Application
import scorex.network.Handshake
import scorex.network.peer.PeerManager
import spray.http.MediaTypes._
import spray.routing.Route

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersHttpService(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers // TODO implement and fix ~ score
    }

  @Path("/all")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def allPeers: Route = path("all") {
    get {
      respondWithMediaType(`application/json`) {
        onComplete {
          (application.peerManager ? PeerManager.GetAllPeers)
            .mapTo[Seq[InetSocketAddress]]
            .map { addresses =>
              val peerData = Json.arr(addresses.map { address =>
                JsString(address.toString)
              })
              Json.obj("peers" -> peerData).toString()
            }
        } {
          case Success(value) => complete(value)
          case Failure(ex) => failWith(ex)
        }
      }
    }
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def connectedPeers: Route = path("connected") {
    get {
      respondWithMediaType(`application/json`) {
        onComplete {
          (application.peerManager ? PeerManager.GetConnectedPeers)
            .mapTo[Seq[Handshake]]
            .map { handshakes =>
              val peerData = Json.arr(handshakes.map { handshake =>
                val s = handshake.fromAddress.toString + ":" + handshake.fromPort + "::" + handshake.fromNonce
                JsString(s)
              })
              Json.obj("peers" -> peerData).toString()
            }
        } {
          case Success(value) => complete(value)
          case Failure(ex) => failWith(ex)
        }
      }
    }
  }

  /*  @Path("/score")
    @ApiOperation(value = "Score", notes = "Node with a maximum blockchain score", httpMethod = "GET")
    @ApiResponses(Array(
      new ApiResponse(code = 200, message = "Json with response or error"),
      new ApiResponse(code = 500, message = "Internal error")
    ))
    def score = path("score") {
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
    }*/

}
