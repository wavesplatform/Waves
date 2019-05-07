package com.wavesplatform.matcher.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.testkit.{TestActor, TestProbe}
import com.google.common.primitives.Longs
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.http.RouteSpec
import com.wavesplatform.matcher.{AddressActor, AddressDirectory, AssetPairBuilder, Matcher}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.{RequestGen, WithDB, crypto}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsString, JsValue}

import scala.concurrent.Future

class MatcherApiRouteSpec extends RouteSpec("/matcher") with RequestGen with PathMockFactory with Eventually with WithDB {

  private val settings          = WavesSettings.fromConfig(ConfigFactory.load())
  private val matcherPrivateKey = PrivateKeyAccount("matcher".getBytes)

  routePath("/balance/reserved/{publicKey}") - {
    val publicKey = matcherPrivateKey.publicKey
    val ts        = System.currentTimeMillis()
    val signature = crypto.sign(matcherPrivateKey, publicKey ++ Longs.toByteArray(ts))

    def mkGet(route: Route)(base58PublicKey: String, ts: Long, base58Signature: String): RouteTestResult =
      Get(routePath(s"/balance/reserved/$base58PublicKey")).withHeaders(
        RawHeader("Timestamp", s"$ts"),
        RawHeader("Signature", base58Signature)
      ) ~> route

    "returns a reserved balance for specified publicKey" in test { route =>
      mkGet(route)(Base58.encode(publicKey), ts, Base58.encode(signature)) ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    "returns HTTP 400 when provided a wrong base58-encoded" - {
      "signature" in test { route =>
        mkGet(route)(Base58.encode(publicKey), ts, ";;") ~> check {
          status shouldBe StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Invalid signature"
        }
      }

      "public key" in test { route =>
        mkGet(route)(";;", ts, Base58.encode(signature)) ~> check {
          handled shouldBe false
        }
      }
    }
  }

  private def test[T](f: Route => T): T = {
    val blockchain   = stub[Blockchain]
    val addressActor = TestProbe("address")
    addressActor.setAutoPilot { (sender: ActorRef, msg: Any) =>
      msg match {
        case AddressDirectory.Envelope(_, AddressActor.GetReservedBalance) => sender ! Map.empty[Option[AssetId], Long]
        case _                                                             =>
      }

      TestActor.NoAutoPilot
    }

    val route = MatcherApiRoute(
      assetPairBuilder = new AssetPairBuilder(settings.matcherSettings, blockchain),
      matcherPublicKey = matcherPrivateKey,
      matcher = ActorRef.noSender,
      addressActor = addressActor.ref,
      storeEvent = _ => Future.failed(new NotImplementedError("Storing is not implemented")),
      orderBook = _ => None,
      getMarketStatus = _ => None,
      orderValidator = _ => Left("Not implemented"),
      orderBookSnapshot = new OrderBookSnapshotHttpCache(settings.matcherSettings.orderBookSnapshotHttpCache, ntpTime, _ => None),
      wavesSettings = settings,
      matcherStatus = () => Matcher.Status.Working,
      db = db,
      time = ntpTime,
      currentOffset = () => 0L,
      lastOffset = () => Future.successful(0L),
      matcherAccountFee = 300000L
    ).route

    f(route)
  }

}
