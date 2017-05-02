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
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, EqByteArray, LeaseInfo, Portfolio}
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionModule
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.{NTP, ScorexLogging, Time, TimeImpl}
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
  val storedState: StateReader = stub[StateReader] // fromDBWithUnlimitedBalance(db, TestBlockchainSettings.Disabled.functionalitySettings)

  val settings = matcherSettings.copy(account = MatcherAccount.address)

  val wallet = new Wallet(None, "matcher", Option(WalletSeed))
  wallet.generateNewAccount()
  var actor: ActorRef = system.actorOf(Props(new MatcherActor(storedState, wallet, settings,
    stub[TransactionModule]) with RestartableActor))

  (storedState.assetInfo _).when(*).returns(Some(AssetInfo(true, 10000000000L)))
  val i1 = IssueTransaction.create(PrivateKeyAccount(Array.empty), "Unknown".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L).right.get
  (storedState.transactionInfo _).when(*).returns(Some((1, i1)))
  (storedState.accountPortfolio _).when(*).returns(Portfolio(Long.MaxValue, LeaseInfo.empty, Map(EqByteArray("123".getBytes) -> Long.MaxValue)))

  override protected def beforeEach() = {
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
    super.beforeEach()

    actor = system.actorOf(Props(new MatcherActor(storedState, wallet, settings,
      stub[TransactionModule]) with RestartableActor))
  }

  "MatcherActor" should {

    "AssetPair with same assets" in {
      def sameAssetsOrder(): Order = Order.apply(PrivateKeyAccount("123".getBytes()), MatcherAccount,
        AssetPair(Some("asset1".getBytes), Some("asset1".getBytes)), OrderType.BUY,
        100000000L, 100L, 1L, 1000L, 100000L)

      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(StatusCodeMatcherResponse(StatusCodes.NotFound, "Invalid AssetPair"))
    }

    "AssetPair with predefined pair" in {
      def predefinedPair = AssetPair(Base58.decode("BASE2").toOption, Base58.decode("BASE1").toOption)

      actor ! GetOrderBookRequest(predefinedPair, None)
      expectMsg(GetOrderBookResponse(predefinedPair, Seq(), Seq()))

      def reversePredefinedPair = AssetPair(Base58.decode("BASE1").toOption, Base58.decode("BASE2").toOption)

      actor ! GetOrderBookRequest(reversePredefinedPair, None)
      expectMsg(StatusCodeMatcherResponse(StatusCodes.Found, "Invalid AssetPair ordering, should be reversed: BASE2-BASE1"))
    }

    "AssetPair with predefined price assets" in {
      def priceAsset = AssetPair(Base58.decode("ABC").toOption, Base58.decode("BASE1").toOption)
      actor ! GetOrderBookRequest(priceAsset, None)
      expectMsg(GetOrderBookResponse(priceAsset, Seq(), Seq()))

      def wrongPriceAsset = AssetPair(Base58.decode("BASE2").toOption, Base58.decode("CDE").toOption)
      actor ! GetOrderBookRequest(wrongPriceAsset, None)
      expectMsg(StatusCodeMatcherResponse(StatusCodes.Found, "Invalid AssetPair ordering, should be reversed: CDE-BASE2"))
    }

    "AssetPair with unknown assets" in {
      def unknownAssets = AssetPair(Base58.decode("Some2").toOption, Base58.decode("Some1").toOption)

      actor ! GetOrderBookRequest(unknownAssets, None)
      expectMsg(GetOrderBookResponse(unknownAssets, Seq(), Seq()))

      def wrongUnknownAssets = AssetPair(Base58.decode("Some1").toOption, Base58.decode("Some2").toOption)

      actor ! GetOrderBookRequest(wrongUnknownAssets, None)
      expectMsg(StatusCodeMatcherResponse(StatusCodes.Found, "Invalid AssetPair ordering, should be reversed: Some2-Some1"))
    }

    "accept orders with AssetPair with same assets" in {
      def sameAssetsOrder(): Order = Order.apply(PrivateKeyAccount("123".getBytes()), MatcherAccount,
        AssetPair(Some.apply("asset1".getBytes), Some.apply("asset1".getBytes)), OrderType.BUY,
        100000000L, 100L, 1L, 1000L, 100000L)

      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(StatusCodeMatcherResponse(StatusCodes.NotFound, "Invalid AssetPair"))
    }

    "restore OrderBook after restart" in {
      val pair = AssetPair(Some("123".getBytes), None)
      val order = buy(pair, 1, 2000)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! RestartActor
      actor ! GetOrderBookRequest(pair, None)
      expectMsg(GetOrderBookResponse(pair, Seq(LevelAgg(100000000, 2000)), Seq()))
    }

    "return all open markets" in {
      val a1 = Some("123".getBytes)
      val a2 = Some("234".getBytes)

      val pair = AssetPair(a2, a1)
      val order = buy(pair, 1, 2000)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! GetMarkets

      val Predefined = AssetPair(Base58.decode("BASE2").toOption, Base58.decode("BASE1").toOption)

      expectMsgPF() {
        case GetMarketsResponse(publicKey, Seq(
        MarketData(`Predefined`, "Unknown", "Unknown", _),
        MarketData(_, "Unknown", "Unknown", _))) =>
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

      val pair1 = AssetPair(a1, None)
      val pair2 = AssetPair(a1, a2)

      val now = NTP.correctedTime()
      val json = GetMarketsResponse(Array(), Seq(MarketData(pair1, a1Name, waves, now),
        MarketData(pair2, a1Name, a2Name, now))).json

      ((json \ "markets") (0) \ "priceAsset").as[String] shouldBe AssetPair.WavesName
      ((json \ "markets") (0) \ "priceAssetName").as[String] shouldBe waves
      ((json \ "markets") (0) \ "amountAsset").as[String] shouldBe Base58.encode(a1.get)
      ((json \ "markets") (0) \ "amountAssetName").as[String] shouldBe a1Name
      ((json \ "markets") (0) \ "created").as[Long] shouldBe now

      ((json \ "markets") (1) \ "amountAssetName").as[String] shouldBe a1Name
    }
  }
}
