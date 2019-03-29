package com.wavesplatform.matcher.model

import com.wavesplatform.NoShrink
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.state.diffs.{CommonValidation, produce}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ExchangeTransactionCreatorSpecification
    extends WordSpec
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val pair = AssetPair(None, mkAssetId("BTC"))

  "ExchangeTransactionCreator" when {
    "SmartAccountTrading hasn't been activated yet" should {
      "create an ExchangeTransactionV1" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L))
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L))

        val bc = stub[Blockchain]
        (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV1]
      }

      "return an error" when {
        List((1, 2), (2, 1), (2, 2)).foreach {
          case (buyVersion, sellVersion) =>
            s"buyV$buyVersion and sellV$sellVersion" in {
              val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = buyVersion.toByte)
              val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = sellVersion.toByte)

              val bc = stub[Blockchain]
              (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
              val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
              tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()) should produce(
                "SmartAccountTrading has not been activated yet")
            }
        }
      }
    }

    "SmartAccountTrading has been activated" should {
      "create an ExchangeTransactionV2" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        val bc = stub[Blockchain]

        (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0)).anyNumberOfTimes()

        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)

        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV2]
      }

      "create an ExchangeTransactionV2 and charge extra fee for scripts or not depends on matcher settings" in {

        val bc           = stub[Blockchain]
        val permitScript = ExprScript(Terms.TRUE).explicitGet()

        def mkAssetDescription(decimals: Int): Option[AssetDescription] = {
          Some(AssetDescription(MatcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0))
        }

        def setAssetScript(asset: ByteStr): Unit = {
          (bc.assetDescription _).when(asset).returns(mkAssetDescription(8))
          (bc.assetScript _).when(asset).returns(Some(permitScript))
          (bc.hasAssetScript _).when(asset).returns(true)
        }

        val amountAsset = mkAssetId("AA")
        val priceAsset  = mkAssetId("PA")
        val assetPair   = AssetPair(amountAsset, priceAsset)

        val counter   = buy(assetPair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(assetPair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        setAssetScript(amountAsset.get)
        setAssetScript(priceAsset.get)

        (bc.accountScript _).when(MatcherAccount.toAddress).returns(None)
        (bc.hasScript _).when(MatcherAccount.toAddress).returns(true)

        (bc.activatedFeatures _)
          .when()
          .returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0, BlockchainFeatures.SmartAssets.id -> 0))
          .anyNumberOfTimes()

        val tcEnableChargingFeeForScripts = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)

        val tx1 =
          tcEnableChargingFeeForScripts.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet()

        tx1 shouldBe a[ExchangeTransactionV2]
        tx1.fee shouldBe (matcherSettings.orderMatchTxFee + CommonValidation.ScriptExtraFee * 3)

        val tcDisableChargingFeeForScripts = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings.copy(disableExtraFeeForScript = true))

        val tx2 =
          tcDisableChargingFeeForScripts.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet()

        tx2 shouldBe a[ExchangeTransactionV2]
        tx2.fee shouldBe matcherSettings.orderMatchTxFee
      }
    }
  }
}
