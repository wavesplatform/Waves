package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.{BuyLimitOrder, LevelAgg, LimitOrder}
import com.wavesplatform.settings.WavesSettings
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import play.api.libs.json.{JsObject, JsString}
import scorex.account.PrivateKeyAccount
import scorex.settings.ChainParameters
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

class MatcherActorSpecification extends TestKit(ActorSystem.apply("MatcherTest"))
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender
  with MatcherTestData
  with BeforeAndAfterEach
  with ScorexLogging
  with PathMockFactory {

  val db = new MVStore.Builder().compress().open()
  val storedState = fromDBWithUnlimitedBalance(db, ChainParameters.Disabled)

  val settings = new WavesSettings(JsObject(Seq(
    "matcher" -> JsObject(
      Seq("account" -> JsString(MatcherAccount.address))
    )
  )))
  val wallet = new Wallet(None, "matcher", Option(WalletSeed))
  wallet.generateNewAccount()
  val actor: ActorRef = system.actorOf(Props(new MatcherActor(storedState, wallet, settings,
    stub[TransactionModule[StoredInBlock]]) with RestartableActor))

  override protected def beforeEach() = {
    super.beforeEach()
  }

  "MatcherActor" should {

    "accept orders with wrong AssetPair" in {
      def sameAssetsOrder(): Order = Order.apply(new PrivateKeyAccount("123".getBytes()), new PrivateKeyAccount("mather".getBytes()),
        Some.apply("asset1".getBytes), Some.apply("asset1".getBytes), 100000000L, 100L, 1L, 1000L, 100000L)

      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(OrderRejected("Invalid AssetPair"))
    }

    "restore OrderBook after restart" in {
      val pair = AssetPair(None, Some("123".getBytes))
      val order = buy(pair, 100000000, 2000)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! RestartActor
      actor ! GetOrderBookRequest(pair, None)
      expectMsg(GetOrderBookResponse(pair, Seq(LevelAgg(100000000,2000)), Seq()))
    }
  }
}
