package com.wavesplatform.matcher.market

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, StorageExtension}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.matcher.api.StatusCodeMatcherResponse
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, GetMarketsResponse, MarketData}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.market.OrderHistoryActor.{ValidateOrder, ValidateOrderResult}
import com.wavesplatform.matcher.model.OrderBook
import com.wavesplatform.matcher.{AssetPairBuilder, MatcherTestData}
import com.wavesplatform.settings.{TestFunctionalitySettings, WalletSettings}
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.{NTP, ScorexLogging}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}

class MatcherActorSpecification
    extends TestKit(ActorSystem.apply("MatcherTest2"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with MatcherTestData
    with BeforeAndAfterEach
    with ScorexLogging
    with PathMockFactory {

  val blockchain: Blockchain = stub[Blockchain]

  val settings = matcherSettings.copy(
    account = MatcherAccount.address,
  )

  val pairBuilder = new AssetPairBuilder(settings, blockchain)

  val functionalitySettings = TestFunctionalitySettings.Stub
  val wallet                = Wallet(WalletSettings(None, "matcher", Some(WalletSeed)))
  wallet.generateNewAccount()

  val orderHistoryRef = TestActorRef(new Actor {
    def receive: Receive = {
      case ValidateOrder(o, _) => sender() ! ValidateOrderResult(ByteStr(o.id()), Right(o))
      case _                   =>
    }
  })

  val obc                                              = new ConcurrentHashMap[AssetPair, OrderBook]
  val ob                                               = new AtomicReference(Map.empty[AssetPair, ActorRef])
  def update(ap: AssetPair)(snapshot: OrderBook): Unit = obc.put(ap, snapshot)

  var actor: ActorRef = system.actorOf(Props(
    new MatcherActor(orderHistoryRef, pairBuilder, ob, update, wallet, mock[UtxPool], mock[ChannelGroup], settings, blockchain, functionalitySettings)
    with RestartableActor))

  val i1 = IssueTransactionV1
    .selfSigned(PrivateKeyAccount(Array.empty), "Unknown".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
    .right
    .get
  val i2 = IssueTransactionV1
    .selfSigned(PrivateKeyAccount(Array.empty), "ForbiddenName".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
    .right
    .get
  (blockchain.assetDescription _)
    .when(i2.id())
    .returns(Some(AssetDescription(i2.sender, "ForbiddenName".getBytes, "".getBytes, 8, false, i2.quantity, None, 0)))
  (blockchain.assetDescription _)
    .when(*)
    .returns(Some(AssetDescription(PublicKeyAccount(Array(0: Byte)), "Unknown".getBytes, "".getBytes, 8, false, i1.quantity, None, 0)))
  (blockchain.portfolio _).when(*).returns(Portfolio(Long.MaxValue, LeaseBalance.empty, Map(ByteStr("123".getBytes) -> Long.MaxValue)))

  override protected def beforeEach(): Unit = {
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
    obc.clear()
    super.beforeEach()

    actor = system.actorOf(
      Props(
        new MatcherActor(orderHistoryRef,
                         pairBuilder,
                         ob,
                         update,
                         wallet,
                         mock[UtxPool],
                         mock[ChannelGroup],
                         settings,
                         blockchain,
                         functionalitySettings) with RestartableActor))
  }

  "MatcherActor" should {

    "AssetPair with same assets" in {
      def sameAssetsOrder(): Order =
        Order.apply(
          PrivateKeyAccount("123".getBytes()),
          MatcherAccount,
          AssetPair(strToSomeAssetId("asset1"), strToSomeAssetId("asset1")),
          OrderType.BUY,
          100000000L,
          100L,
          1L,
          1000L,
          100000L,
          1: Byte
        )

      val invalidOrder = sameAssetsOrder()
      actor ! invalidOrder
      expectMsg(StatusCodeMatcherResponse(StatusCodes.NotFound, "Amount and price assets must be different"))
    }

    "Reject order when script fails" in {
      val sender = PrivateKeyAccount("Trader#1".getBytes)

      val script = ScriptCompiler(
        """
          |match tx {
          |  case _: Order => false
          |  case _ => false
          |}
        """.stripMargin
      ).explicitGet()._1

      val order = Order(
        sender,
        MatcherAccount,
        AssetPair(strToSomeAssetId("asset#2"), strToSomeAssetId("asset#1")),
        OrderType.BUY,
        100000000L,
        100L,
        1L,
        1000L,
        100000L,
        1: Byte
      )

      (blockchain.accountScript _)
        .when(order.sender.toAddress)
        .returns(Some(script))

      (blockchain.accountScript _)
        .when(MatcherAccount.toAddress)
        .returns(None)

      actor ! order
      expectMsg(StatusCodeMatcherResponse(StatusCodes.Forbidden, "Order not allowed by sender script"))
    }

    "return all open markets" in {
      val a1 = strToSomeAssetId("123")
      val a2 = strToSomeAssetId("234")

      val pair  = AssetPair(a2, a1)
      val order = buy(pair, 1, 2000)

      (blockchain.accountScript _)
        .when(order.sender.toAddress)
        .returns(None)

      (blockchain.accountScript _)
        .when(order.matcherPublicKey.toAddress)
        .returns(None)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! GetMarkets

      expectMsgPF() {
        case GetMarketsResponse(publicKey, Seq(MarketData(_, "Unknown", "Unknown", _, _, _))) =>
          publicKey shouldBe MatcherAccount.publicKey
      }
    }
  }

  "GetMarketsResponse" should {
    "serialize to json" in {
      val waves  = "WAVES"
      val a1Name = "BITCOIN"
      val a1     = strToSomeAssetId(a1Name)

      val a2Name = "US DOLLAR"
      val a2     = strToSomeAssetId(a2Name)

      val pair1 = AssetPair(a1, None)
      val pair2 = AssetPair(a1, a2)

      val now = NTP.correctedTime()
      val json =
        GetMarketsResponse(Array(), Seq(MarketData(pair1, a1Name, waves, now, None, None), MarketData(pair2, a1Name, a2Name, now, None, None))).json

      ((json \ "markets")(0) \ "priceAsset").as[String] shouldBe AssetPair.WavesName
      ((json \ "markets")(0) \ "priceAssetName").as[String] shouldBe waves
      ((json \ "markets")(0) \ "amountAsset").as[String] shouldBe a1.get.base58
      ((json \ "markets")(0) \ "amountAssetName").as[String] shouldBe a1Name
      ((json \ "markets")(0) \ "created").as[Long] shouldBe now

      ((json \ "markets")(1) \ "amountAssetName").as[String] shouldBe a1Name
    }
  }

  def strToSomeAssetId(s: String): Option[AssetId] = Some(ByteStr(s.getBytes()))
}
