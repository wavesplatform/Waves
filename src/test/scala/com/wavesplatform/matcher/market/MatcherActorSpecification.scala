package com.wavesplatform.matcher.market

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, InMemorySnapshotStorage, StorageExtension}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse, MarketData}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.{BuyLimitOrder, LevelAgg, LimitOrder}
import com.wavesplatform.settings.WavesSettings
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import play.api.libs.json.{JsNull, JsObject, JsString, Json}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
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
  var actor: ActorRef = system.actorOf(Props(new MatcherActor(storedState, wallet, settings,
    stub[TransactionModule]) with RestartableActor))

  override protected def beforeEach() = {
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
    super.beforeEach()

    actor = system.actorOf(Props(new MatcherActor(storedState, wallet, settings,
      stub[TransactionModule[StoredInBlock]]) with RestartableActor))
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

    "return all open markets" in {
      val a1 = Some("123".getBytes)
      val a2 = Some("234".getBytes)

      val pair = AssetPair(a1, a2)
      val order = buy(pair, 100000000, 2000)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! GetMarkets

      expectMsgPF() {
        case GetMarketsResponse(Seq(MarketData(_, "Unknown","Unknown", _))) =>
      }
    }
  }

  "GetMarketsResponse" should {
    "serialize to json" in {
      val waves = "WAVES"
      val a1Name = "BITCOIN"
      val a1 = Some(a1Name.getBytes)

      val a2Name = "US DOLLAR"
      val a2 = Some(a2Name.getBytes)

      val pair1 = AssetPair(None, a1)
      val pair2 = AssetPair(a1, a2)

      val now =  LocalDateTime.now().withNano(0)
      val json = GetMarketsResponse(Seq(MarketData(pair1, waves, a1Name, now),
        MarketData(pair2, a1Name, a2Name, now))).json

      println(Json.prettyPrint(json))
      ((json \ "result") (0) \ "firstAssetId").get shouldBe JsNull
      ((json \ "result") (0) \ "firstAssetName").as[String] shouldBe waves
      ((json \ "result") (0) \ "secondAssetId").as[String] shouldBe Base58.encode(a1.get)
      ((json \ "result") (0) \ "secondAssetName").as[String] shouldBe a1Name
      ((json \ "result") (0) \ "created").as[String] shouldBe now.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

      ((json \ "result") (1) \ "secondAssetName").as[String] shouldBe a2Name
    }
  }
}
