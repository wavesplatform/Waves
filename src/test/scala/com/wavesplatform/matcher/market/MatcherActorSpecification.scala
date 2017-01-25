package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.market.OrderBookActor.OrderRejected
import com.wavesplatform.settings.WavesSettings
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import play.api.libs.json.{JsObject, JsString}
import scorex.account.PrivateKeyAccount
import scorex.settings.WavesHardForkParameters
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.Order
import scorex.transaction.state.database.blockchain.StoredState
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
  val storedState = StoredState.fromDB(db, WavesHardForkParameters.Disabled)

  val settings = new WavesSettings(JsObject(Seq(
    "matcher" -> JsObject(
      Seq("account" -> JsString(MatcherAccount.address))
    )
  )))
  val wallet = new Wallet(None, "matcher", Option(WalletSeed))
  wallet.generateNewAccount()
  val actor: ActorRef = system.actorOf(MatcherActor.props(storedState, wallet, settings,
    stub[TransactionModule[StoredInBlock]]), MatcherActor.name)

  override protected def beforeEach() = {
    super.beforeEach()
  }

  "MatcherActor" should {
    def sameAssetsOrder(): Order = Order.apply(new PrivateKeyAccount("123".getBytes()), new PrivateKeyAccount("mather".getBytes()),
      Some.apply("asset1".getBytes), Some.apply("asset1".getBytes), 100000000L, 100L, 1L, 100000L)

    "accept orders with wrong AssetPair" in {
      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(OrderRejected("Invalid AssetPair"))
    }
  }
}
