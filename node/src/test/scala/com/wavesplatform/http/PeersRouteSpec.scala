package com.wavesplatform.http

import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.api.http.PeersApiRoute
import com.wavesplatform.network.{PeerDatabase, PeerInfo}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks as PropertyChecks
import play.api.libs.json.{Format, JsObject, JsValue, Json}

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.{Duration, DurationInt}

class PeersRouteSpec extends RouteSpec("/peers") with RestAPISettingsHelper with PropertyChecks {

  import PeersRouteSpec.*

  private val peerDatabase   = PeerDatabase.NoOp
  private val inetAddressGen = Gen.listOfN(4, Arbitrary.arbitrary[Byte]).map(_.toArray).map(InetAddress.getByAddress)
  private val inetSocketAddressGen = for {
    address <- inetAddressGen
    port    <- Gen.chooseNum(0, 0xffff)
  } yield new InetSocketAddress(address, port)

  private val versionGen = for {
    major <- Gen.chooseNum(0, 3)
    minor <- Gen.chooseNum(0, 3)
    patch <- Gen.chooseNum(0, 3)
  } yield (major, minor, patch)

  private implicit val timeout: Duration = 2.seconds

  private def genListOf[A](maxLength: Int, src: Gen[A]) = Gen.chooseNum(0, maxLength).flatMap(n => Gen.listOfN(n, src))

  routePath("/connected") in {
    val gen = for {
      remoteAddress      <- inetSocketAddressGen
      declaredAddress    <- Gen.option(inetSocketAddressGen)
      nodeName           <- Gen.alphaNumStr
      nodeNonce          <- Arbitrary.arbitrary[Int]
      applicationName    <- Gen.alphaNumStr
      applicationVersion <- versionGen
    } yield PeerInfo(remoteAddress, declaredAddress, applicationName, applicationVersion, nodeName, nodeNonce)

    forAll(genListOf(TestsCount, gen)) { (l: List[PeerInfo]) =>
      val connections = new ConcurrentHashMap[Channel, PeerInfo]()
      val route       = PeersApiRoute(restAPISettings, ???, peerDatabase, connections).route
      l.foreach(i => connections.put(new EmbeddedChannel(), i))

      val result = Get(routePath("/connected")) ~> route ~> runRoute

      check {
        responseAs[Connected].peers should contain theSameElementsAs l.map { pi =>
          ConnectedPeer(
            pi.remoteAddress.toString,
            pi.declaredAddress.fold("N/A")(_.toString),
            pi.nodeName,
            pi.nodeNonce,
            pi.applicationName,
            s"${pi.applicationVersion._1}.${pi.applicationVersion._2}.${pi.applicationVersion._3}"
          )
        }
      }(result)
    }
  }

  routePath("/all") in {
    val gen = for {
      inetAddress <- inetSocketAddressGen
      ts          <- Gen.posNum[Long]
    } yield inetAddress -> ts

    forAll(genListOf(TestsCount, gen)) { m =>
      val route  = PeersApiRoute(restAPISettings, ???, peerDatabase, new ConcurrentHashMap[Channel, PeerInfo]()).route
      val result = Get(routePath("/all")) ~> route ~> runRoute

      check {
        responseAs[AllPeers].peers should contain theSameElementsAs m.map { case (address, timestamp) =>
          Peer(address.toString, timestamp)
        }
      }(result)
    }
  }

  routePath("/connect") in {
    val route      = PeersApiRoute(restAPISettings, ???, peerDatabase, new ConcurrentHashMap[Channel, PeerInfo]()).route
    val connectUri = routePath("/connect")
    Post(connectUri, ConnectReq("example.com", 1)) ~> route should produce(ApiKeyNotValid)
    Post(connectUri, "") ~> ApiKeyHeader ~> route ~> check(handled shouldEqual false)
    Post(connectUri, Json.obj()) ~> ApiKeyHeader ~> route ~> check {
      (responseAs[JsValue] \ "validationErrors").as[JsObject].keys should not be empty
    }

    val address = inetSocketAddressGen.sample.get
    val result  = Post(connectUri, ConnectReq(address.getHostName, address.getPort)) ~> ApiKeyHeader ~> route ~> runRoute
    check {
      responseAs[ConnectResp].hostname shouldEqual address.getHostName
    }(result)
  }

}

object PeersRouteSpec {
  val TestsCount = 20

  case class ConnectReq(host: String, port: Int)

  implicit val connectReqFormat: Format[ConnectReq] = Json.format

  case class ConnectResp(status: String, hostname: String)

  implicit val connectRespFormat: Format[ConnectResp] = Json.format

  case class ConnectedPeer(
      address: String,
      declaredAddress: String,
      peerName: String,
      peerNonce: Long,
      applicationName: String,
      applicationVersion: String
  )

  implicit val connectedPeerFormat: Format[ConnectedPeer] = Json.format

  case class Connected(peers: Seq[ConnectedPeer])

  implicit val connectedFormat: Format[Connected] = Json.format

  case class Peer(address: String, lastSeen: Long)

  implicit val peerFormat: Format[Peer] = Json.format

  case class BlacklistedPeer(hostname: String, timestamp: Long, reason: String)

  implicit val blacklistedPeerFormat: Format[BlacklistedPeer] = Json.format

  case class AllPeers(peers: Seq[Peer])

  implicit val allPeersFormat: Format[AllPeers] = Json.format
}
