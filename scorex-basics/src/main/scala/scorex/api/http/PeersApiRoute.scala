package scorex.api.http

import java.net.{InetAddress, InetSocketAddress}
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.swagger.annotations._
import play.api.libs.json.{JsArray, JsString, Json}
import scorex.app.RunnableApplication
import scorex.network.Handshake
import scorex.network.NetworkController.ConnectTo
import scorex.network.peer.{PeerInfo, PeerManager}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

@Path("/peers")
@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute {
  val MaxPeersInResponse = 1000

  val settings = application.settings

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
    getJsonRoute {
      (application.peerManager ? PeerManager.GetAllPeers)
        .mapTo[Map[InetSocketAddress, PeerInfo]]
        .map { peers =>
          JsonResponse(
            JsArray(peers.take(MaxPeersInResponse).map { case (address, peerInfo) =>
              Json.obj(
                "address" -> address.toString,
                "nodeName" -> peerInfo.nodeName,
                "nodeNonce" -> peerInfo.nonce.toString,
                "lastSeen" -> peerInfo.timestamp
              )
            }.toList),
            StatusCodes.OK
          )
        }
    }
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def connectedPeers: Route = path("connected") {
    getJsonRoute {
      (application.peerManager ? PeerManager.GetConnectedPeers)
        .mapTo[List[(InetSocketAddress, Handshake)]]
        .map { connectedPeers =>
          val peerData = JsArray(connectedPeers.take(MaxPeersInResponse).map { peer =>
            Json.obj(
              "address" -> peer._1.toString,
              "declaredAddress" -> (peer._2.declaredAddress.map(_.toString).getOrElse("N/A"): String),
              "peerName" -> peer._2.nodeName,
              "peerNonce" -> peer._2.nodeNonce
            )
          })
          JsonResponse(Json.obj("peers" -> peerData), StatusCodes.OK)
        }
    }
  }

  @Path("/connect")
  @ApiOperation(value = "Connect to peer", notes = "Connect to peer", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "string",
      defaultValue = "{\n\t\"host\":\"127.0.0.1\",\n\t\"port\":\"9084\"\n}"
    )
  )) def connect: Route = path("connect") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try {
            val js = Json.parse(body)
            val host = (js \ "host").as[String]
            val port = (js \ "port").as[Int]
            val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(host), port)
            application.networkController ! ConnectTo(add)

            JsonResponse(Json.obj("hostname" -> add.getHostName, "status" -> "Trying to connect"), StatusCodes.OK)
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }

  @Path("/blacklisted")
  @ApiOperation(value = "Blacklisted peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def blacklistedPeers: Route = path("blacklisted") {
    getJsonRoute {
      (application.peerManager ? PeerManager.GetBlacklistedPeers)
        .mapTo[Set[String]]
        .map { peers =>
          JsonResponse(JsArray(peers.take(MaxPeersInResponse).map(JsString).toSeq), StatusCodes.OK)
        }
    }
  }
}
