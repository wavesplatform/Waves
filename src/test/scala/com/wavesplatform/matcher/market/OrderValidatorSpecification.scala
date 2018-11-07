package com.wavesplatform.matcher.market

import com.google.common.base.Charsets
import com.wavesplatform.OrderOps._
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled}
import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
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
  private val defaultTs    = 1000

  private val defaultPortfolio = Portfolio(0, LeaseBalance.empty, Map(wbtc -> 10 * Constants.UnitsInWave))

  "OrderValidator" should {
    "allow buying WAVES for BTC without balance for order fee" in
      portfolioTest(defaultPortfolio) { (ov, bc) =>
        val o = newBuyOrder
        (bc.accountScript _).when(o.sender.toAddress).returns(None)
        ov.validateNewOrder(o) shouldBe 'right
      }

    "reject new order" when {
      "asset balance is negative" in portfolioTest(Portfolio(0, LeaseBalance.empty, Map(wbtc -> -10 * Constants.UnitsInWave))) { (ov, _) =>
        ov.validateNewOrder(newBuyOrder) should produce("Not enough tradable balance")
      }

      "this order had already been accepted" in portfolioTest(defaultPortfolio) { (ov, _) =>
        val o       = newBuyOrder
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(o)))
        ov.validateNewOrder(o) shouldBe Left("Order has already been placed")
      }

      "this order had already been canceled" in portfolioTest(defaultPortfolio) { (ov, _) =>
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

      "v1 order from a scripted account" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ScriptV1(Terms.TRUE).explicitGet()))
          (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 100))
          (bc.height _).when().returns(50).once()

          ov.validateNewOrder(newBuyOrder(scripted)) should produce("Trading on scripted account isn't allowed yet")
        }
      }

      "sender's address has a script, but trading from smart accounts hasn't been activated" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (ov, bc) =>
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ScriptV1(Terms.TRUE).explicitGet()))
          (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 100))
          (bc.height _).when().returns(50).once()

          ov.validateNewOrder(newBuyOrder(scripted)) should produce("Trading on scripted account isn't allowed yet")
        }
      }

      "sender's address has a script returning FALSE" in forAll(accountGen) { scripted =>
        portfolioTest(defaultPortfolio) { (_, bc) =>
          (bc.accountScript _).when(scripted.toAddress).returns(Some(ScriptV1(Terms.FALSE).explicitGet()))
          (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 100))
          (bc.height _).when().returns(150).once()

          val ov = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, ntpTime)
          ov.validateNewOrder(newBuyOrder(scripted, version = 2)) should produce("Order rejected by script")
        }
      }

      "order expires too soon" in forAll(Gen.choose[Long](1, OrderValidator.MinExpiration), accountGen) { (offset, pk) =>
        val bc       = stub[Blockchain]
        val tt       = new TestTime
        val ov       = new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), matcherSettings, MatcherAccount, tt)
        val unsigned = newBuyOrder
        val signed   = Order.sign(unsigned.updateExpiration(tt.getTimestamp() + offset).updateSender(pk), pk)
        ov.validateNewOrder(signed) shouldBe Left("Order expiration should be > 1 min")
      }

      "amount is invalid" in portfolioTest(defaultPortfolio) { (ov, _) =>
        val pk = PrivateKeyAccount(randomBytes())
        val unsigned = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(amount = 0L)
          case x: OrderV2 => x.copy(amount = 0L)
        }
        val signed = Order.sign(unsigned, pk)
        ov.validateNewOrder(signed) should produce("amount should be > 0")
      }

      "order signature is invalid" in portfolioTest(defaultPortfolio) { (ov, bc) =>
        val pk = PrivateKeyAccount(randomBytes())
        (bc.accountScript _).when(pk.toAddress).returns(None)
        val order = newBuyOrder(pk) match {
          case x: OrderV1 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
          case x: OrderV2 => x.copy(proofs = Proofs(Seq(ByteStr(Array.emptyByteArray))))
        }
        ov.validateNewOrder(order) should produce("Script doesn't exist and proof doesn't validate as signature")
      }

      "default ts - drift > its for new users" in portfolioTest(defaultPortfolio) { (ov, _) =>
        ov.validateNewOrder(newBuyOrder(defaultTs - matcherSettings.orderTimestampDrift - 1)) should produce("Order should have a timestamp")
      }

      "default ts - drift = its ts for new users" in portfolioTest(defaultPortfolio) { (ov, _) =>
        ov.validateNewOrder(newBuyOrder(defaultTs - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }

      "ts1 - drift > ts2" in portfolioTest(defaultPortfolio) { (ov, _) =>
        val pk      = PrivateKeyAccount(randomBytes())
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(newBuyOrder(pk, defaultTs + 1000))))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 999 - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }

      "ts1 - drift = ts2" in portfolioTest(defaultPortfolio) { (ov, _) =>
        val pk      = PrivateKeyAccount(randomBytes())
        val history = new OrderHistory(db, matcherSettings)
        history.process(OrderAdded(LimitOrder(newBuyOrder(pk, defaultTs + 1000))))
        ov.validateNewOrder(newBuyOrder(pk, defaultTs + 1000 - matcherSettings.orderTimestampDrift)) should produce("Order should have a timestamp")
      }

      "order price has invalid non-zero trailing decimals" in forAll(assetIdGen(1), accountGen, Gen.choose(1, 7)) {
        case (Some(amountAsset), sender, amountDecimals) =>
          portfolioTest(Portfolio(11 * Constants.UnitsInWave, LeaseBalance.empty, Map.empty)) { (ov, bc) =>
            (bc.hasScript _).when(sender.toAddress).returns(false)
            (bc.assetDescription _).when(amountAsset).returns(mkAssetDescription(amountDecimals))

            val price = BigDecimal(10).pow(-amountDecimals - 1)
            ov.validateNewOrder(buy(AssetPair(Some(amountAsset), None),
                                    10 * Constants.UnitsInWave,
                                    price,
                                    matcherFee = Some((0.003 * Constants.UnitsInWave).toLong))) should produce("Invalid price")

          }
      }
    }

    "validate order with any number of signatures from a scripted account" in forAll(Gen.choose(0, 5)) { proofsNumber =>
      validateOrderProofsTest((1 to proofsNumber).map(x => ByteStr(Array(x.toByte))))
    }

    "meaningful error for undefined functions in matcher" in portfolioTest(defaultPortfolio) { (ov, bc) =>
      (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0)).anyNumberOfTimes()

      val pk     = PrivateKeyAccount(randomBytes())
      val o      = newBuyOrder(pk, version = 2)
      val script = ScriptCompiler("true && (height > 0)").explicitGet()._1
      (bc.accountScript _).when(pk.toAddress).returns(Some(script))
      ov.validateNewOrder(o) should produce("height is inaccessible when running script on matcher")
    }
  }

  private def portfolioTest(p: Portfolio)(f: (OrderValidator, Blockchain) => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()
    f(new OrderValidator(db, bc, _ => p, Right(_), matcherSettings, MatcherAccount, ntpTime), bc)
  }

  private def settingsTest(settings: MatcherSettings)(f: OrderValidator => Any): Unit = {
    val bc = stub[Blockchain]
    (bc.assetDescription _).when(wbtc).returns(mkAssetDescription(8)).anyNumberOfTimes()
    f(new OrderValidator(db, bc, _ => defaultPortfolio, Right(_), settings, MatcherAccount, ntpTime))
  }

  private def validateOrderProofsTest(proofs: Seq[ByteStr]): Unit = portfolioTest(defaultPortfolio) { (ov, bc) =>
    val pk            = PrivateKeyAccount(randomBytes())
    val accountScript = ScriptV1(ScriptVersion.Versions.V2, Terms.TRUE, checkSize = false).explicitGet()

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

  private def mkAssetDescription(decimals: Int): Option[AssetDescription] =
    Some(AssetDescription(MatcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0))

  private def newBuyOrder: Order =
    buy(pair = pairWavesBtc, amount = 100 * Constants.UnitsInWave, price = 0.0022, matcherFee = Some((0.003 * Constants.UnitsInWave).toLong))

  private def newBuyOrder(ts: Long): Order =
    buy(pair = pairWavesBtc,
        amount = 100 * Constants.UnitsInWave,
        price = 0.0022,
        matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
        ts = Some(ts))

  private def newBuyOrder(pk: PrivateKeyAccount, ts: Long = System.currentTimeMillis(), version: Byte = 1) =
    buy(
      pair = pairWavesBtc,
      amount = 100 * Constants.UnitsInWave,
      price = 0.0022,
      sender = Some(pk),
      matcherFee = Some((0.003 * Constants.UnitsInWave).toLong),
      ts = Some(ts),
      version = version
    )

  private def mkAssetId(prefix: String): ByteStr = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32))
  }
}
