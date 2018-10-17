package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.WithDB
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import com.wavesplatform.settings.{Constants, WalletSettings}
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with PropertyChecks
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with PathMockFactory {

  private val bc: Blockchain = stub[Blockchain]
  private val i1: IssueTransactionV1 =
    IssueTransactionV1
      .selfSigned(PrivateKeyAccount(Array.empty), "WBTC".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
      .right
      .get
  (bc.transactionInfo _).when(*).returns(Some((1, i1)))

  private val defaultTs: Long    = 1000
  private val s: MatcherSettings = matcherSettings.copy(account = MatcherAccount.address, defaultOrderTimestamp = defaultTs)
  private val w                  = Wallet(WalletSettings(None, Some("matcher"), Some(WalletSeed)))
  w.generateNewAccount()

  private var ov = new OrderValidator {
    override val orderHistory: OrderHistory = new OrderHistory(db, s)
    override val utxPool: UtxPool           = stub[UtxPool]
    override val settings: MatcherSettings  = s
    override val wallet: Wallet             = w
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    ov = new OrderValidator {
      override val orderHistory: OrderHistory = new OrderHistory(db, s)
      override val utxPool: UtxPool           = stub[UtxPool]
      override val settings: MatcherSettings  = s
      override val wallet: Wallet             = w
    }
  }

  val wbtc: ByteStr = mkAssetId("WBTC")
  val pairWavesBtc  = AssetPair(None, Some(wbtc))

  "OrderValidator" should {
    "allows a new order" when {
      "buy WAVES for BTC without balance for order fee" in {
        validateNewOrderTest(
          Portfolio(0,
                    LeaseBalance.empty,
                    Map(
                      wbtc -> 10 * Constants.UnitsInWave
                    ))) shouldBe an[Right[_, _]]
      }

      "default ts < its ts for new users" in {
        setupEnoughPortfolio()
        ov.validateNewOrder(newBuyOrder(defaultTs + 1)) shouldBe 'right
      }

      "ts1 < ts2" in {
        setupEnoughPortfolio()
        val pk = PrivateKeyAccount(randomBytes())

        ov.orderHistory.setLastOrder(pk, newBuyOrder(pk, defaultTs + 1))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 2)) shouldBe 'right
      }
    }

    "does not allow to add an order" when {
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

      "the limit has been reached" in {
        import DBUtils.indexes.active.MaxElements
        setupEnoughPortfolio()

        val pk = PrivateKeyAccount("foo".getBytes())
        (1 to MaxElements).foreach { i =>
          val o = newBuyOrder(pk, i)
          ov.orderHistory.process(OrderAdded(LimitOrder(o)))
        }

        ov.validateNewOrder(newBuyOrder(pk, 1000)) shouldBe Left(GenericError(s"Limit of $MaxElements active orders has been reached"))
      }

      "default ts > its for new users" in {
        setupEnoughPortfolio()
        ov.validateNewOrder(newBuyOrder(defaultTs - 1)) should produce("Order should have a timestamp")
      }

      "default ts = its ts for new users" in {
        setupEnoughPortfolio()
        ov.validateNewOrder(newBuyOrder(defaultTs)) should produce("Order should have a timestamp")
      }

      "ts1 > ts2" in {
        setupEnoughPortfolio()
        val pk = PrivateKeyAccount(randomBytes())

        ov.orderHistory.setLastOrder(pk, newBuyOrder(pk, defaultTs + 2))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 1)) should produce("Order should have a timestamp")
      }

      "ts1 = ts2" in {
        setupEnoughPortfolio()
        val pk = PrivateKeyAccount(randomBytes())

        ov.orderHistory.setLastOrder(pk, newBuyOrder(pk, defaultTs + 1))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 1)) should produce("Order should have a timestamp")
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
    amount = 100 * Constants.UnitsInWave,
    price = 0.0022,
    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong)
  )

  private def newBuyOrder(ts: Long): Order = buy(
    pair = pairWavesBtc,
    amount = 100 * Constants.UnitsInWave,
    price = 0.0022,
    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
    ts = Some(ts)
  )

  private def newBuyOrder(pk: PrivateKeyAccount, ts: Long): Order = buy(
    pair = pairWavesBtc,
    amount = 100 * Constants.UnitsInWave,
    price = 0.0022,
    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
    sender = Some(pk),
    ts = Some(ts)
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
