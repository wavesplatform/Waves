package com.wavesplatform.transaction.smart

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier.ValidationResult
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.{NTPTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class VerifierSpecification extends PropSpec with PropertyChecks with Matchers with MockFactory with TransactionGen with NTPTime {
  private val estimator = ScriptEstimatorV2

  property("ExchangeTransaction - blockchain's functions should be allowed during an order's verification") {
    forAll(exchangeTransactionV2Gen) { tx: ExchangeTransaction =>
      val bc = stub[Blockchain]
      Seq(tx.buyOrder.assetPair.amountAsset, tx.buyOrder.assetPair.priceAsset)
        .collect { case asset: IssuedAsset => asset }
        .foreach { asset =>
          (bc.assetDescription _).when(asset).returns(mkAssetDescription(tx.sender, 8))
          (bc.assetScript _).when(asset).returns(None)
          (bc.activatedFeatures _).when().returns(Map())
        }

      val scriptText =
        """match tx {
          |  case o: Order => height >= 0
          |  case _ => true
          |}""".stripMargin
      val script = ScriptCompiler(scriptText, isAssetScript = false, estimator).explicitGet()._1

      (bc.accountScript _).when(tx.sellOrder.sender.toAddress).returns(None)
      (bc.accountScript _).when(tx.buyOrder.sender.toAddress).returns(Some(script))
      (bc.accountScript _).when(tx.sender.toAddress).returns(None)

      (bc.height _).when().returns(0)

      Verifier(bc, 0)(tx).resultE shouldBe 'right
    }
  }

  property("ExchangeTransaction - matcherFeeAssetId's in custom exchange transaction should be verified") {
    forAll(exchangeTransactionV2WithArbitraryFeeAssetsInOrdersGen) { tx =>
      val (invalidScript, _) = ScriptCompiler.compile("(5 / 0) == 2", estimator).explicitGet()
      val falseScript        = ExprScript(Terms.FALSE).explicitGet()

      setFeeAssetScriptsAndVerify(Some(invalidScript), None)(tx) should produce("ScriptExecutionError")        // buy order:  matcherFeeAsset has invalid script
      setFeeAssetScriptsAndVerify(None, Some(falseScript))(tx) should produce("TransactionNotAllowedByScript") // sell order: matcherFeeAsset script gives false as a result
      setFeeAssetScriptsAndVerify(None, None)(tx) shouldBe 'right
    }
  }

  private def mkAssetDescription(matcherAccount: PublicKey, decimals: Int): Option[AssetDescription] =
    Some(AssetDescription(matcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0))

  private val exchangeTransactionV2Gen: Gen[ExchangeTransaction] = for {
    sender1: KeyPair <- accountGen
    sender2: KeyPair <- accountGen
    assetPair                  <- assetPairGen
    r                          <- exchangeV2GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset, orderVersions = Set(2, 3))
  } yield r

  private def setFeeAssetScriptsAndVerify(buyFeeAssetScript: Option[Script], sellFeeAssetScript: Option[Script])(
      tx: ExchangeTransaction): ValidationResult[Transaction] = {

    val blockchain = stub[Blockchain]

    def activate(features: (BlockchainFeature, Int)*): Unit = {
      (blockchain.activatedFeatures _).when().returns(features.map(x => x._1.id -> x._2).toMap).anyNumberOfTimes()
    }

    activate(BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0, BlockchainFeatures.SmartAssets -> 0)

    def prepareAssets(assetsAndScripts: (Asset, Option[Script])*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption) =>
        (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(tx.sender, 8))
        (blockchain.assetScript _).when(asset).returns(scriptOption)
        (blockchain.hasAssetScript _).when(asset).returns(scriptOption.isDefined)
      case _ =>
    }

    prepareAssets(
      tx.buyOrder.matcherFeeAssetId     -> buyFeeAssetScript,
      tx.sellOrder.matcherFeeAssetId    -> sellFeeAssetScript,
      tx.buyOrder.assetPair.amountAsset -> None,
      tx.buyOrder.assetPair.priceAsset  -> None
    )

    Seq(tx.sellOrder.sender.toAddress, tx.buyOrder.sender.toAddress, tx.sender.toAddress) foreach { address =>
      (blockchain.accountScript _).when(address).returns(None)
    }

    (blockchain.height _).when().returns(0)

    Verifier(blockchain, 0)(tx).resultE
  }
}
