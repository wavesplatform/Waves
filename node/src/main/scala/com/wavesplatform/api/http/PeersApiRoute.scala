package com.wavesplatform.api.http

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentMap
import java.util.stream.Collectors

import akka.http.scaladsl.server.Route
import com.wavesplatform.network.{PeerDatabase, PeerInfo}
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.Channel
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._

import scala.collection.JavaConverters._

@Path("/peers")
@Api(value = "/peers")
case class PeersApiRoute(settings: RestAPISettings,
                         connectToPeer: InetSocketAddress => Unit,
                         peerDatabase: PeerDatabase,
                         establishedConnections: ConcurrentMap[Channel, PeerInfo])
    extends ApiRoute
    with AuthRoute {

  import PeersApiRoute._

  override lazy val route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ suspendedPeers ~ connect ~ clearBlacklist
    }

  @Path("/all")
  @ApiOperation(value = "Peer list", notes = "Peer list", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json with peer list or error")
    ))
  def allPeers: Route = (path("all") & get) {
    complete(
      Json.obj(
        "peers" ->
          JsArray(peerDatabase.knownPeers
            .take(MaxPeersInResponse)
            .map {
              case (address, timestamp) =>
                Json.obj(
                  "address"  -> address.toString,
                  "lastSeen" -> timestamp
                )
            }
            .toList)))
  }

  @Path("/connected")
  @ApiOperation(value = "Connected peers list", notes = "Connected peers list", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json with connected peers or error")
    ))
  def connectedPeers: Route = (path("connected") & get) {
    val peers = establishedConnections
      .values()
      .stream()
      .map[JsValue](pi =>
        Json.obj(
          "address"            -> pi.remoteAddress.toString,
          "declaredAddress"    -> pi.declaredAddress.fold("N/A")(_.toString),
          "peerName"           -> pi.nodeName,
          "peerNonce"          -> pi.nodeNonce,
          "applicationName"    -> pi.applicationName,
          "applicationVersion" -> s"${pi.applicationVersion._1}.${pi.applicationVersion._2}.${pi.applicationVersion._3}"
      ))
      .collect(Collectors.toList())
      .asScala

    complete(Json.obj("peers" -> JsArray(peers)))
  }

  @Path("/connect")
  @ApiOperation(value = "Connect to peer", notes = "Connect to peer", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.ConnectReq"
      )
    ))
  def connect: Route = (path("connect") & withAuth) {
    jsonPost[ConnectReq] { req =>
      val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(req.host), req.port)
      connectToPeer(add)

      Json.obj("hostname" -> add.getHostName, "status" -> "Trying to connect")
    }
  }

  @Path("/blacklisted")
  @ApiOperation(value = "Blacklisted peers list", notes = "Blacklisted peers list", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json with blacklisted peers or error")
    ))
  def blacklistedPeers: Route = (path("blacklisted") & get) {
    complete(
      JsArray(
        peerDatabase.detailedBlacklist
          .take(MaxPeersInResponse)
          .map { case (h, (t, r)) => Json.obj("hostname" -> h.toString, "timestamp" -> t, "reason" -> r) }
          .toList))
  }

  @Path("/suspended")
  @ApiOperation(value = "Suspended peers list", notes = "Suspended peers list", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "JSON with suspended peers or error")
    ))
  def suspendedPeers: Route = (path("suspended") & get) {
    complete(
      JsArray(
        peerDatabase.detailedSuspended.take(MaxPeersInResponse).map { case (h, t) => Json.obj("hostname" -> h.toString, "timestamp" -> t) }.toList))
  }

  @Path("/clearblacklist")
  @ApiOperation(value = "Remove all blacklisted peers", notes = "Clear blacklist", httpMethod = "POST")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "200")
    ))
  def clearBlacklist: Route = (path("clearblacklist") & post & withAuth) {
    peerDatabase.clearBlacklist()
    complete(Json.obj("result" -> "blacklist cleared"))
  }
}

object PeersApiRoute {
  val MaxPeersInResponse = 1000
}
