package scorex.api.http

import java.net.{InetAddress, InetSocketAddress}
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.pattern.ask
import com.wordnik.swagger.annotations._
import play.api.libs.json.{JsArray, JsString, Json}
import scorex.app.Application
import scorex.network.Handshake
import scorex.network.NetworkController.ConnectTo
import scorex.network.peer.{PeerInfo, PeerManager}
import spray.routing.Route

import scala.concurrent.ExecutionContext.Implicits.global

@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonApiFunctions {

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ connect
    }

  @Path("/all")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def allPeers: Route = path("all") {
    jsonRoute {
      (application.peerManager ? PeerManager.GetAllPeers)
        .mapTo[Map[InetSocketAddress, PeerInfo]]
        .map { peers =>
          Json.arr(peers.map { case (address, peerInfo) =>
            Json.obj(
              "address" -> address.toString,
              "nodeName" -> (peerInfo.nodeName.getOrElse("N/A"): String),
              "nodeNonce" -> (peerInfo.nonce.map(_.toString).getOrElse("N/A"): String)
            )
          }).toString()
        }
    }
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def connectedPeers: Route = path("connected") {
    jsonRoute {
      (application.peerManager ? PeerManager.GetConnectedPeers)
        .mapTo[Seq[Handshake]]
        .map { handshakes =>
          val peerData = Json.arr(handshakes.map { handshake =>
            Json.obj(
              "declaredAddress" -> handshake.declaredAddress.toString,
              "peerName" -> handshake.nodeName,
              "peerNonce" -> handshake.nodeNonce
            )
          })
          Json.obj("peers" -> peerData).toString()
        }
    }
  }

  @Path("/connect")
  @ApiOperation(value = "Connect to peer", notes = "Connect to peer", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "host", value = "Peer host", required = true, paramType = "body", dataType = "String"),
    new ApiImplicitParam(name = "port", value = "Peer port", required = true, paramType = "body", dataType = "Int")
  ))
  def connect: Route = path("connect") {
    val host: String = "127.0.0.1"
    val port: Int = 9084

    incompletedJsonRoute(
      entity(as[String]) { body => complete {
        val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port)
        application.networkController ! ConnectTo(add)
        Json.obj(add.getHostName -> "Trying to connect").toString()
      }
      }, post)
  }

  @Path("/blacklisted")
  @ApiOperation(value = "Blacklisted peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def blacklistedPeers: Route = path("blacklisted") {
    jsonRoute {
      (application.peerManager ? PeerManager.GetBlacklistedPeers)
        .mapTo[Seq[String]]
        .map { peers =>
          JsArray(peers.map(i => JsString(i))).toString()
        }
    }
  }


}
