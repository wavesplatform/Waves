package com.wavesplatform.api.http

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentMap
import java.util.stream.Collectors

import akka.http.scaladsl.server.Route
import com.wavesplatform.network.{PeerDatabase, PeerInfo}
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.Channel
import play.api.libs.json.*

import scala.jdk.CollectionConverters.*

case class PeersApiRoute(
    settings: RestAPISettings,
    connectToPeer: InetSocketAddress => Unit,
    peerDatabase: PeerDatabase,
    establishedConnections: ConcurrentMap[Channel, PeerInfo]
) extends ApiRoute
    with AuthRoute {

  import PeersApiRoute.*

  override lazy val route: Route =
    pathPrefix("peers") {
      allPeers ~ connectedPeers ~ blacklistedPeers ~ suspendedPeers ~ connect ~ clearBlacklist
    }

  def allPeers: Route = (path("all") & get) {
    complete(
      Json.obj(
        "peers" ->
          JsArray(
            peerDatabase.knownPeers
              .take(MaxPeersInResponse)
              .map { case (address, timestamp) =>
                Json.obj(
                  "address"  -> address.toString,
                  "lastSeen" -> timestamp
                )
              }
              .toList
          )
      )
    )
  }

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
        )
      )
      .collect(Collectors.toList())
      .asScala

    complete(Json.obj("peers" -> JsArray(peers)))
  }

  def connect: Route = (path("connect") & withAuth) {
    jsonPost[ConnectReq] { req =>
      val add: InetSocketAddress = new InetSocketAddress(InetAddress.getByName(req.host), req.port)
      connectToPeer(add)

      Json.obj("hostname" -> add.getHostName, "status" -> "Trying to connect")
    }
  }

  def blacklistedPeers: Route = (path("blacklisted") & get) {
    complete(
      JsArray(
        peerDatabase.detailedBlacklist
          .take(MaxPeersInResponse)
          .map { case (h, (t, r)) => Json.obj("hostname" -> h.toString, "timestamp" -> t, "reason" -> r) }
          .toList
      )
    )
  }

  def suspendedPeers: Route = (path("suspended") & get) {
    complete(
      JsArray(
        peerDatabase.detailedSuspended.take(MaxPeersInResponse).map { case (h, t) => Json.obj("hostname" -> h.toString, "timestamp" -> t) }.toList
      )
    )
  }

  def clearBlacklist: Route = (path("clearblacklist") & post & withAuth) {
    peerDatabase.clearBlacklist()
    complete(Json.obj("result" -> "blacklist cleared"))
  }
}

object PeersApiRoute {
  val MaxPeersInResponse = 1000
}
