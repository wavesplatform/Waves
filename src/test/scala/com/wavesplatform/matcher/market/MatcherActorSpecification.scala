package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, StorageExtension}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.StatusCodeMatcherResponse
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse, MarketData}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.LevelAgg
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.settings.TestChainParameters
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.{NTP, ScorexLogging}
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
  val storedState = fromDBWithUnlimitedBalance(db, TestChainParameters.Disabled)

  val settings = matcherSettings.copy(account = MatcherAccount.address)

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
      stub[TransactionModule]) with RestartableActor))
  }

  "MatcherActor" should {

    "accept orders with wrong AssetPair" in {
      def sameAssetsOrder(): Order = Order.apply(new PrivateKeyAccount("123".getBytes()), MatcherAccount,
        Some.apply("asset1".getBytes), Some.apply("asset1".getBytes), 100000000L, 100L, 1L, 1000L, 100000L)

      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(StatusCodeMatcherResponse(StatusCodes.NotFound, "Invalid AssetPair"))
    }

    "restore OrderBook after restart" in {
      val pair = AssetPair(None, Some("123".getBytes))
      val order = buy(pair, 100000000, 2000)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! RestartActor
      actor ! GetOrderBookRequest(pair, None)
      expectMsg(GetOrderBookResponse(pair, Seq(LevelAgg(100000000, 2000)), Seq()))
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
        case GetMarketsResponse(publicKey, Seq(MarketData(_, "Unknown", "Unknown", _))) =>
          publicKey shouldBe MatcherAccount.publicKey
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

      val now = NTP.correctedTime()
      val json = GetMarketsResponse(Array(), Seq(MarketData(pair1, waves, a1Name, now),
        MarketData(pair2, a1Name, a2Name, now))).json

      ((json \ "markets") (0) \ "asset1Id").as[String] shouldBe AssetPair.WavesName
      ((json \ "markets") (0) \ "asset1Name").as[String] shouldBe waves
      ((json \ "markets") (0) \ "asset2Id").as[String] shouldBe Base58.encode(a1.get)
      ((json \ "markets") (0) \ "asset2Name").as[String] shouldBe a1Name
      ((json \ "markets") (0) \ "created").as[Long] shouldBe now

      ((json \ "markets") (1) \ "asset2Name").as[String] shouldBe a2Name
    }
  }
}
