package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.WithDB
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import com.wavesplatform.settings.{Constants, WalletSettings}
import com.wavesplatform.state.{Blockchain, ByteStr, EitherExt2, LeaseBalance, Portfolio}
import com.wavesplatform.utx.UtxPool
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.wallet.Wallet

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with PropertyChecks
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with PathMockFactory {

  val utxPool: UtxPool = stub[UtxPool]

  val bc: Blockchain = stub[Blockchain]
  val i1: IssueTransactionV1 =
    IssueTransactionV1
      .selfSigned(PrivateKeyAccount(Array.empty), "WBTC".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
      .right
      .get
  (bc.transactionInfo _).when(*).returns(Some((1, i1)))

  val s: MatcherSettings             = matcherSettings.copy(account = MatcherAccount.address)
  val w                              = Wallet(WalletSettings(None, Some("matcher"), Some(WalletSeed)))
  val acc: Option[PrivateKeyAccount] = w.generateNewAccount()

  val matcherPubKey: PublicKeyAccount = w.findPrivateKey(s.account).explicitGet()

  private var ov = new OrderValidator {
    override val orderHistory: OrderHistory = new OrderHistory(db, matcherSettings)
    override val utxPool: UtxPool           = stub[UtxPool]
    override val settings: MatcherSettings  = s
    override val wallet: Wallet             = w
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    ov = new OrderValidator {
      override val orderHistory: OrderHistory = new OrderHistory(db, matcherSettings)
      override val utxPool: UtxPool           = stub[UtxPool]
      override val settings: MatcherSettings  = s
      override val wallet: Wallet             = w
    }
  }

  val wbtc: ByteStr = mkAssetId("WBTC")
  val pairWavesBtc  = AssetPair(None, Some(wbtc))

  "OrderValidator" should {
    "allows buy WAVES for BTC without balance for order fee" in {
      validateNewOrderTest(
        Portfolio(0,
                  LeaseBalance.empty,
                  Map(
                    wbtc -> 10 * Constants.UnitsInWave
                  ))) shouldBe an[Right[_, _]]
    }

    "does not allow to add the order" when {
      "the assets number is negative" in {
        validateNewOrderTest(
          Portfolio(0,
                    LeaseBalance.empty,
                    Map(
                      wbtc -> -10 * Constants.UnitsInWave
                    ))) shouldBe a[Left[_, _]]
      }

      "it was added before" when {
        "it accepted" in {
          val o = newBuyOrder
          setupEnoughPortfolio()

          ov.orderHistory.process(OrderAdded(LimitOrder(o)))

          ov.validateNewOrder(o) shouldBe Left(GenericError("Order was placed before"))
        }

        "it canceled" in {
          val o = newBuyOrder
          setupEnoughPortfolio()

          ov.orderHistory.process(OrderAdded(LimitOrder(o)))
          ov.orderHistory.process(OrderCanceled(LimitOrder(o), unmatchable = false))

          ov.validateNewOrder(o) shouldBe Left(GenericError("Order was placed before"))
        }
      }
    }
  }

  private def validateNewOrderTest(expectedPortfolio: Portfolio): Either[ValidationError.GenericError, Order] = {
    (ov.utxPool.portfolio _).when(*).returns(expectedPortfolio)
    val o = newBuyOrder
    ov.validateNewOrder(o)
  }

  private def newBuyOrder: Order = buy(
    pair = pairWavesBtc,
    price = 0.0022,
    amount = 100 * Constants.UnitsInWave,
    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong)
  )

  private def setupEnoughPortfolio(): Unit = {
    (ov.utxPool.portfolio _)
      .when(*)
      .returns(
        Portfolio(0,
                  LeaseBalance.empty,
                  Map(
                    wbtc -> 10 * Constants.UnitsInWave
                  )))
  }

  private def mkAssetId(prefix: String): ByteStr = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32))
  }
}
