package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.OrderOps._
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV2}
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
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

  (bc.accountScript _).when(MatcherAccount.toAddress).returns(None).anyNumberOfTimes()

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in
      portfolioTest(defaultPortfolio) { ov =>
        val o = newBuyOrder
        (bc.accountScript _).when(o.sender.toAddress).returns(None)
        ov.validateNewOrder(o) shouldBe 'right
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

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        (bc.accountScript _).when(scripted.toAddress).returns(Some(ScriptV1(Terms.TRUE).explicitGet()))
        (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 100))
        (bc.height _).when().returns(50).once()

        val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
        ov.validateNewOrder(newBuyOrder(scripted)) should produce("Trading on scripted account isn't allowed yet")
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        (bc.accountScript _).when(scripted.toAddress).returns(Some(ScriptV1(Terms.FALSE).explicitGet()))
        (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 100))
        (bc.height _).when().returns(150).once()

        val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
        ov.validateNewOrder(newBuyOrder(scripted)) should produce("Order rejected by script")
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val tt       = new TestTime
        val ov       = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, tt)
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp() + offset).updateSender(pk), pk)
        ov.validateNewOrder(signed) shouldBe Left("Order expiration should be > 1 min")
      }

      "order signature is invalid" in {
        val o = newBuyOrder.updateProofs(Proofs(List(ByteStr(new Array[Byte](32)))))
        (bc.accountScript _).when(o.senderPublicKey.toAddress).returns(None)
        val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
        ov.validateNewOrder(o) should produce("Script doesn't exist and proof doesn't validate as signature")
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

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }
  }

  private def portfolioTest(p: Portfolio)(f: OrderValidator => Any): Unit = {
    f(new OrderValidator(db, bc, _ => p, Right(_), matcherSettings, MatcherAccount, ntpTime))
  }

  private def settingsTest(settings: MatcherSettings)(f: OrderValidator => Any): Unit = {
    f(new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), settings, MatcherAccount, ntpTime))
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = portfolioTest(defaultPortfolio) { ov =>
    val pk            = PrivateKeyAccount(randomBytes())
    val accountScript = ScriptV1(TRUE, checkSize = false).explicitGet()

    (bc.accountScript _).when(pk.toAddress).returns(Some(accountScript)).anyNumberOfTimes()
    (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0)).anyNumberOfTimes()
    (bc.height _).when().returns(1).anyNumberOfTimes()

    val order = OrderV2(
      senderPublicKey = pk,
      matcherPublicKey = MatcherAccount,
      assetPair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = (0.0022 * Order.PriceConstant).toLong,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 60 * 60 * 1000L,
      matcherFee = (0.003 * Constants.UnitsInWave).toLong,
      orderType = OrderType.BUY,
      proofs = Proofs.empty
    )

    ov.validateNewOrder(order) shouldBe 'right
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
