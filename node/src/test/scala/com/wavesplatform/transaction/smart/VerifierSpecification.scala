package com.wavesplatform.transaction.smart

import com.google.protobuf.ByteString
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.{BlockchainSettings, GenesisSettings, RewardsSettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Blockchain, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier.ValidationResult
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
          (bc.assetDescription _).when(asset).returns(mkAssetDescription(asset.id, tx.sender, 8, None))
          (bc.assetScript _).when(asset).returns(None)
          (() => bc.activatedFeatures).when().returns(Map())
          (() => bc.settings).when().returns(BlockchainSettings(
            addressSchemeCharacter = 'N',
            functionalitySettings = TestFunctionalitySettings.Enabled,
            genesisSettings = GenesisSettings.TESTNET,
            rewardsSettings = RewardsSettings.TESTNET
          ))
        }

      val scriptText =
        """match tx {
          |  case _: Order => height >= 0
          |  case _ => true
          |}""".stripMargin
      val (script, complexity) = ScriptCompiler(scriptText, isAssetScript = false, estimator).explicitGet()

      (bc.accountScript _).when(tx.sellOrder.sender.toAddress).returns(None)
      (bc.accountScript _).when(tx.buyOrder.sender.toAddress).returns(Some(AccountScriptInfo(tx.buyOrder.sender, script, complexity)))
      (bc.accountScript _).when(tx.sender.toAddress).returns(None)

      (() => bc.height).when().returns(0)

      verify(bc, tx).explicitGet()
    }
  }

  property("ExchangeTransaction - matcherFeeAssetId's in custom exchange transaction should be verified") {
    forAll(exchangeTransactionV2WithArbitraryFeeAssetsInOrdersGen) { tx =>
      val (invalidScript, comp) = ScriptCompiler.compile("(5 / 0) == 2", estimator).explicitGet()
      val falseScript           = ExprScript(Terms.FALSE).explicitGet()

      setFeeAssetScriptsAndVerify(Some(invalidScript -> comp), None)(tx) should produce("ScriptExecutionError") // buy order:  matcherFeeAsset has invalid script
      setFeeAssetScriptsAndVerify(None, Some((falseScript, 1)))(tx) should produce("TransactionNotAllowedByScript") // sell order: matcherFeeAsset script gives false as a result
      setFeeAssetScriptsAndVerify(None, None)(tx).explicitGet()
    }
  }

  private def mkAssetDescription(
      assetId: ByteStr,
      matcherAccount: PublicKey,
      decimals: Int,
      scriptOption: Option[(Script, Long)]
  ): Option[AssetDescription] =
    Some(
      AssetDescription(
        assetId,
        matcherAccount,
        ByteString.EMPTY,
        ByteString.EMPTY,
        decimals,
        reissuable = false,
        BigInt(0),
        Height(0),
        scriptOption.map(AssetScriptInfo.tupled),
        0,
        decimals == 0
      )
    )

  private val exchangeTransactionV2Gen: Gen[ExchangeTransaction] = for {
    sender1: KeyPair <- accountGen
    sender2: KeyPair <- accountGen
    assetPair        <- assetPairGen
    r                <- exchangeV2GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset, orderVersions = Set(2, 3))
  } yield r

  private def setFeeAssetScriptsAndVerify(buyFeeAssetScript: Option[(Script, Long)], sellFeeAssetScript: Option[(Script, Long)])(
      tx: ExchangeTransaction
  ): ValidationResult[Transaction] = {

    val blockchain = stub[Blockchain]
    (() => blockchain.settings).when().returns(BlockchainSettings(
      addressSchemeCharacter = 'N',
      functionalitySettings = TestFunctionalitySettings.Enabled,
      genesisSettings = GenesisSettings.TESTNET,
      rewardsSettings = RewardsSettings.TESTNET
    ))

    def activate(features: (BlockchainFeature, Int)*): Unit = {
      (() => blockchain.activatedFeatures).when().returns(features.map(x => x._1.id -> x._2).toMap).anyNumberOfTimes()
    }

    activate(BlockchainFeatures.SmartAccountTrading -> 0, BlockchainFeatures.OrderV3 -> 0, BlockchainFeatures.SmartAssets -> 0)

    def prepareAssets(assetsAndScripts: (Asset, Option[(Script, Long)])*): Unit = assetsAndScripts foreach {
      case (asset: IssuedAsset, scriptOption) =>
        (blockchain.assetDescription _).when(asset).returns(mkAssetDescription(asset.id, tx.sender, 8, scriptOption))
        (blockchain.assetScript _).when(asset).returns(scriptOption.map(AssetScriptInfo.tupled))
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

    (() => blockchain.height).when().returns(0)

    verify(blockchain, tx)
  }

  def verify(blockchain: Blockchain, tx: Transaction): ValidationResult[Transaction] =
    (for {
      _ <- Verifier(blockchain)(tx)
      _ <- Verifier.assets(blockchain, Int.MaxValue)(tx).leftMap { case (_, ve) => ve }
    } yield tx).resultE

}
