package scorex.api.http

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentMap
import java.util.stream.Collectors
import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.wavesplatform.network.{PeerDatabase, PeerInfo}
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.Channel
import io.swagger.annotations._
import play.api.libs.json._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

@Path("/peers")
@Api(value = "/peers", description = "Get info about peers", position = 2)
case class PeersApiRoute(
    settings: RestAPISettings,
    connectToPeer: InetSocketAddress => Unit,
    peerDatabase: PeerDatabase,
    establishedConnections: ConcurrentMap[Channel, PeerInfo]) extends ApiRoute {
  import PeersApiRoute._

  private implicit val timeout: Timeout = 5.seconds

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ connect
    }

  @Path("/all")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def allPeers: Route = (path("all") & get) {
    complete(Json.obj("peers" ->
      JsArray(peerDatabase.getKnownPeers.take(MaxPeersInResponse).map { case (address, peerInfo) =>
        Json.obj(
          "address" -> address.toString,
          "nodeName" -> peerInfo.nodeName,
          "nodeNonce" -> peerInfo.nonce,
          "lastSeen" -> peerInfo.timestamp
        )
      }.toList)))
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def connectedPeers: Route = (path("connected") & get) {
    val peers = establishedConnections.values().stream().map[JsValue](pi => Json.obj(
      "address" -> pi.remoteAddress.toString,
      "declaredAddress" -> pi.declaredAddress.fold("N/A")(_.toString),
      "peerName" -> pi.nodeName,
      "peerNonce" -> pi.nodeNonce,
      "applicationName" -> pi.applicationName,
      "applicationVersion" -> s"${pi.applicationVersion._1}.${pi.applicationVersion._2}.${pi.applicationVersion._3}"
    )).collect(Collectors.toList()).asScala

    complete(Json.obj("peers" -> JsArray(peers)))
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
  ))
  def connect: Route = (path("connect") & post & withAuth) {
    json[ConnectReq] { req =>
      val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(req.host), req.port)
      connectToPeer(add)

      Json.obj("hostname" -> add.getHostName, "status" -> "Trying to connect")
    }
  }

  @Path("/blacklisted")
  @ApiOperation(value = "Blacklisted peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with connected peers or error")
  ))
  def blacklistedPeers: Route = (path("blacklisted") & get) {
    complete(JsArray(peerDatabase.getBlacklist.take(MaxPeersInResponse).map(a => JsString(a.toString)).toSeq))
  }
}

object PeersApiRoute {
  val MaxPeersInResponse = 1000

  case class ConnectReq(host: String, port: Int)
  implicit val connectFormat: Format[ConnectReq] = Json.format
}
