package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.randomBytes
import com.wavesplatform.{TestTime, WithDB}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks {

  private val wbtc         = mkAssetId("WBTC")
  private val pairWavesBtc = AssetPair(None, Some(wbtc))
  private val bc           = stub[Blockchain]
  private val defaultTs    = 1000

  private def portfolioTest(p: Portfolio)(f: OrderValidator => Any): Unit = {
    f(new OrderValidator(db, bc, _ => p, Right(_), matcherSettings, MatcherAccount, ntpTime))
  }

  private def settingsTest(settings: MatcherSettings)(f: OrderValidator => Any): Unit = {
    f(new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), settings, MatcherAccount, ntpTime))
  }

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in
      portfolioTest(defaultPortfolio) { ov =>
        ov.validateNewOrder(newBuyOrder) shouldBe 'right
      }

    "reject new order" when {
      "asset balance is negative" in {
        portfolioTest(Portfolio(0, LeaseBalance.empty, Map(wbtc -> -10 * Constants.UnitsInWave))) { ov =>
          ov.validateNewOrder(newBuyOrder) should produce("Not enough tradable balance")
        }
      }

      "this order had already been accepted" in portfolioTest(defaultPortfolio) { ov =>
        val o       = newBuyOrder
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(o)))
        ov.validateNewOrder(o) shouldBe Left("Order has already been placed")
      }

      "this order had already been canceled" in portfolioTest(defaultPortfolio) { ov =>
        val o       = newBuyOrder
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(o)))
        history.process(OrderCanceled(LimitOrder(o), unmatchable = false))

        ov.validateNewOrder(o) shouldBe Left("Order has already been placed")
      }

      val blacklistedAccount = PrivateKeyAccount("3irbW78fffj5XDzAMjaEeo3kn8V".getBytes(Charsets.UTF_8))
      "sender's address is blacklisted" in settingsTest(matcherSettings.copy(blacklistedAddresses = Set(blacklistedAccount.toAddress))) { ov =>
        val o = newBuyOrder(blacklistedAccount)
        ov.validateNewOrder(o) shouldBe Left("Invalid address")

      }

      "sender's address has a script" in forAll(accountGen, accountGen) {
        case (scripted, nonScripted) =>
          (bc.hasScript _).when(scripted.toAddress).returns(true).once()
          (bc.hasScript _).when(nonScripted.toAddress).returns(false).once()
          val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
          ov.validateNewOrder(newBuyOrder(scripted)) should produce("")
          ov.validateNewOrder(newBuyOrder(nonScripted)) shouldBe 'right
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val ov       = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, tt)
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.copy(senderPublicKey = pk, expiration = tt.getTimestamp() + offset), pk)
        ov.validateNewOrder(signed) shouldBe Left("Order expiration should be > 1 min")
      }

      "order signature is invalid" in {
        val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
        ov.validateNewOrder(newBuyOrder.copy(signature = Array.emptyByteArray)) shouldBe Left("Invalid signature")
      }

      "default ts - drift > its for new users" in portfolioTest(defaultPortfolio) { ov =>
        ov.validateNewOrder(newBuyOrder(defaultTs - matcherSettings.orderTimestampDrift - 1)) should produce("Order should have a timestamp")
      }

      "default ts - drift = its ts for new users" in portfolioTest(defaultPortfolio) { ov =>
        ov.validateNewOrder(newBuyOrder(defaultTs - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }

      "ts1 - drift > ts2" in portfolioTest(defaultPortfolio) { ov =>
        val pk      = PrivateKeyAccount(randomBytes())
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(newBuyOrder(pk, defaultTs + 1000))))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 999 - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }

      "ts1 - drift = ts2" in portfolioTest(defaultPortfolio) { ov =>
        val pk      = PrivateKeyAccount(randomBytes())
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(newBuyOrder(pk, defaultTs + 1000))))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 1000 - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }
    }
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

  private def newBuyOrder(pk: PrivateKeyAccount, ts: Long = System.currentTimeMillis()): Order = buy(
    pair = pairWavesBtc,
    amount = 100 * Constants.UnitsInWave,
    price = 0.0022,
    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
    sender = Some(pk),
    ts = Some(ts)
  )

  private def mkAssetId(prefix: String): ByteStr = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32))
  }
}
