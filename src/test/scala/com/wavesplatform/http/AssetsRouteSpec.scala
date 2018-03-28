package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.{RequestGen, TestTime}
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.Writes
import scorex.account.Address
import scorex.api.http.assets.{AssetsApiRoute, TransferRequest, VersionedTransferRequest}
import scorex.transaction.Transaction
import scorex.wallet.Wallet

class AssetsRouteSpec extends RouteSpec("/assets") with RequestGen with PathMockFactory with Eventually {

  private val Waves: Long = 100000000L

  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val wallet      = stub[Wallet]
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]
  private val state       = stub[SnapshotStateReader]

  private val seed               = "seed".getBytes()
  private val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
  private val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

  private val apiKeyHeader = api_key("ridethewaves!")

  (wallet.privateKeyAccount _).when(senderPrivateKey.toAddress).onCall((_: Address) => Right(senderPrivateKey)).anyNumberOfTimes()
  (utx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()
  (allChannels.writeAndFlush(_: Any)).when(*).onCall((_: Any) => null).anyNumberOfTimes()

  "/transfer" - {
    val route = AssetsApiRoute(settings, wallet, utx, allChannels, state, new TestTime()).route

    def posting[A: Writes](v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(apiKeyHeader) ~> route

    "accepts TransferRequest" in {
      val req = TransferRequest(
        assetId = None,
        feeAssetId = None,
        amount = 1 * Waves,
        fee = Waves / 3,
        sender = senderPrivateKey.address,
        attachment = None,
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        val r = responseAs[String]
        r should include(""""type" : 4""")
        r shouldNot include("version")
      }
    }

    "accepts VersionedTransferRequest" in {
      val req = VersionedTransferRequest(
        version = 2,
        assetId = None,
        amount = 1 * Waves,
        fee = Waves / 3,
        sender = senderPrivateKey.address,
        attachment = None,
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        val r = responseAs[String]
        r should include(""""type" : 4""")
        r should include(""""version" : 2""")
      }
    }

    "returns a error if it is not a transfer request" in {
      val req = issueReq.sample.get
      posting(req) ~> check {
        status shouldNot be(StatusCodes.OK)
      }
    }
  }

}
