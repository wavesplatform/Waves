package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.assets.{AssetsApiRoute, TransferV1Request, TransferV2Request}
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{RequestGen, TestTime}
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture, ChannelMatcher}
import monix.execution.Scheduler
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.Writes

class AssetsRouteSpec extends RouteSpec("/assets") with RequestGen with PathMockFactory with Eventually with RestAPISettingsHelper {

  private val wallet      = stub[Wallet]
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]
  private val state       = stub[Blockchain]

  private val seed               = "seed".getBytes("UTF-8")
  private val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
  private val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

  (wallet.privateKeyAccount _).when(senderPrivateKey.toAddress).onCall((_: Address) => Right(senderPrivateKey)).anyNumberOfTimes()

  (utx.putIfNew _)
    .when(*, *)
    .onCall((_: Transaction, _: Boolean) => TracedResult(Right(true)))
    .anyNumberOfTimes()

  (allChannels.writeAndFlush(_: Any, _: ChannelMatcher)).when(*, *).onCall((_: Any, _: ChannelMatcher) => stub[ChannelGroupFuture]).anyNumberOfTimes()

  "/transfer" - {
    val route: Route = AssetsApiRoute(restAPISettings, wallet, utx, allChannels, state, new TestTime())(Scheduler(executor)).route

    def posting[A: Writes](v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

    "accepts TransferRequest" in {
      val req = TransferV1Request(
        assetId = None,
        feeAssetId = None,
        amount = 1 * Waves,
        fee = Waves / 3,
        sender = senderPrivateKey.address,
        attachment = Some("attachment"),
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransactionV1]
      }
    }

    "accepts VersionedTransferRequest" in {
      val req = TransferV2Request(
        assetId = None,
        amount = 1 * Waves,
        feeAssetId = None,
        fee = Waves / 3,
        sender = senderPrivateKey.address,
        attachment = None,
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferV2Request]
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
