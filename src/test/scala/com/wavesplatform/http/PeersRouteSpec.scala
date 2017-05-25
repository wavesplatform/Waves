package com.wavesplatform.http

import java.net.{InetAddress, InetSocketAddress}

import akka.testkit.TestProbe
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.network.Handshake
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, JsObject, JsValue, Json}
import scorex.api.http.{ApiKeyNotValid, PeersApiRoute}
import scorex.network.NetworkController.ConnectTo
import scorex.network.peer.PeerInfo
import scorex.network.peer.PeerManager.{GetAllPeers, GetBlacklistedPeers, GetConnectedPeers}

class PeersRouteSpec extends RouteSpec("/peers") with RestAPISettingsHelper with PropertyChecks {
  import PeersRouteSpec._

  private val peerManager = TestProbe()
  private val networkController = TestProbe()
  private val route = PeersApiRoute(restAPISettings, _ => {}, ???, ???).route

  private val inetAddressGen = Gen.listOfN(4, Arbitrary.arbitrary[Byte]).map(_.toArray).map(InetAddress.getByAddress)
  private val inetSocketAddressGen = for {
    address <- inetAddressGen
    port <- Gen.chooseNum(0, 0xFFFF)
  } yield new InetSocketAddress(address, port)

  private def genListOf[A](maxLength: Int, src: Gen[A]) = Gen.chooseNum(0, maxLength).flatMap(n => Gen.listOfN(n, src))

  routePath("/connected") in {
    val gen = for {
      inetAddress <- inetSocketAddressGen
      ts <- Gen.posNum[Long]
      nonce <- Arbitrary.arbitrary[Int]
    } yield inetAddress -> Handshake("", (0, 0, 0), "", nonce, None)

    forAll(genListOf(20, gen)) { l =>
      val result = Get(routePath("/connected")) ~> route ~> runRoute
      peerManager.expectMsg(GetConnectedPeers)
      peerManager.reply(l)

      check {
        responseAs[Connected].peers should contain theSameElementsAs l.map {
          case (address, hs) => ConnectedPeer(address.toString, hs.declaredAddress.fold("N/A")(_.toString), hs.nodeName, hs.nodeNonce)
        }
      }(result)
    }
  }

  routePath("/all") in {
    val gen = for {
      inetAddress <- inetSocketAddressGen
      ts <- Gen.posNum[Long]
      nonce <- Arbitrary.arbitrary[Int]
      nodeName <- Gen.alphaNumStr
    } yield inetAddress -> PeerInfo(ts, nonce, nodeName)

    forAll(genListOf(20, gen)) { m =>
      val result = Get(routePath("/all")) ~> route ~> runRoute
      peerManager.expectMsg(GetAllPeers)
      peerManager.reply(m.toMap)

      check {
        responseAs[AllPeers].peers should contain theSameElementsAs m.map {
          case (address, pi) => Peer(address.toString, pi.nodeName, pi.nonce, pi.timestamp)
        }
      }(result)
    }
  }

  routePath("/connect") in {
    val connectUri = routePath("/connect")
    Post(connectUri, ConnectReq("example.com", 8080)) ~> route should produce (ApiKeyNotValid)
    Post(connectUri, "") ~> api_key(apiKey) ~> route ~> check(handled shouldEqual false)
    Post(connectUri, Json.obj()) ~> api_key(apiKey) ~> route ~> check {
      (responseAs[JsValue] \ "validationErrors").as[JsObject].keys should not be 'empty
    }

    forAll(inetSocketAddressGen) { address =>
      val result = Post(connectUri, ConnectReq(address.getHostName, address.getPort)) ~> api_key(apiKey) ~> route ~> runRoute
      networkController.expectMsg(ConnectTo(address))

      check {
        responseAs[ConnectResp].hostname shouldEqual address.getHostName
      }(result)
    }
  }

  routePath("/blacklisted") in {
    forAll(genListOf(20, inetSocketAddressGen)) { addresses =>
      val addressSet = addresses.map(_.toString).toSet
      val result = Get(routePath("/blacklisted")) ~> route ~> runRoute
      peerManager.expectMsg(GetBlacklistedPeers)
      peerManager.reply(addressSet)

      check {
        responseAs[Seq[String]] should contain theSameElementsAs addressSet
      }(result)
    }
  }
}

object PeersRouteSpec {
  case class ConnectReq(host: String, port: Int)
  implicit val connectReqFormat: Format[ConnectReq] = Json.format

  case class ConnectResp(status: String, hostname: String)
  implicit val connectRespFormat: Format[ConnectResp] = Json.format

  case class ConnectedPeer(address: String, declaredAddress: String, peerName: String, peerNonce: Long)
  implicit val connectedPeerFormat: Format[ConnectedPeer] = Json.format

  case class Connected(peers: Seq[ConnectedPeer])
  implicit val connectedFormat: Format[Connected] = Json.format

  case class Peer(address: String, nodeName: String, nodeNonce: Long, lastSeen: Long)
  implicit val peerFormat: Format[Peer] = Json.format

  case class AllPeers(peers: Seq[Peer])
  implicit val allPeersFormat: Format[AllPeers] = Json.format
}
