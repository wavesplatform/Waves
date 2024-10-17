package com.wavesplatform.transaction.smart

import com.wavesplatform.NTPTime
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.assets.{IssueTransaction, SetAssetScriptTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class VerifierSpecification extends PropSpec with NTPTime with WithDomain {
  private def mkIssue(issuer: KeyPair, name: String, script: Option[Script] = None) =
    IssueTransaction
      .selfSigned(
        2.toByte,
        issuer,
        name,
        "",
        100000_00,
        2,
        reissuable = false,
        script,
        1.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def mkOrder(
      sender: KeyPair,
      orderType: OrderType,
      matcher: PublicKey,
      assetPair: AssetPair,
      fee: Long = 0.003.waves,
      feeAsset: Asset = Waves
  ) =
    Order
      .selfSigned(
        3.toByte,
        sender,
        matcher,
        assetPair,
        orderType,
        100,
        5.waves,
        ntpTime.getTimestamp(),
        ntpTime.getTimestamp() + 200000,
        fee,
        feeAsset
      )
      .explicitGet()

  private val sharedParamGen = for {
    sender  <- accountGen
    matcher <- accountGen
    g1      <- genesisGeneratorP(sender.toAddress)
    g2      <- genesisGeneratorP(matcher.toAddress)
    issuedAsset = mkIssue(sender, "a001")
  } yield (
    sender,
    matcher,
    Seq(g1, g2, issuedAsset),
    AssetPair(IssuedAsset(issuedAsset.id()), Waves)
  )

  property("blockchain functions are available for order branch when verifying exchange transaction") {
    forAll(sharedParamGen) {
      case (sender, matcher, genesisTxs, assetPair) =>
        withDomain(
          domainSettingsWithPreactivatedFeatures(
            BlockchainFeatures.SmartAccountTrading,
            BlockchainFeatures.SmartAssets,
            BlockchainFeatures.OrderV3,
            BlockchainFeatures.Ride4DApps
          )
        ) { d =>
          d.appendBlock(genesisTxs*)
          d.appendBlock(
            SetScriptTransaction
              .selfSigned(
                1.toByte,
                sender,
                Some(
                  ScriptCompiler.compile(
                    """match tx {
                    |  case _: Order => height >= 0
                    |  case _ => true
                    |}""".stripMargin,
                    ScriptEstimatorV2
                  ).explicitGet()._1
                ),
                0.001.waves,
                ntpTime.getTimestamp()
              )
              .explicitGet()
          )

          d.appendBlock(
            ExchangeTransaction
              .signed(
                2.toByte,
                matcher.privateKey,
                mkOrder(sender, OrderType.BUY, matcher.publicKey, assetPair),
                mkOrder(sender, OrderType.SELL, matcher.publicKey, assetPair),
                100,
                5.waves,
                0.003.waves,
                0.003.waves,
                0.003.waves,
                ntpTime.getTimestamp()
              )
              .explicitGet()
          )
        }
    }
  }

  private val sharedParamGen2 = for {
    (sender, matcher, genesisTxs, assetPair) <- sharedParamGen
    buyFeeAssetTx  = mkIssue(sender, "BUYFEE", Some(ExprScript(Terms.TRUE).explicitGet()))
    buyFeeAssetId  = IssuedAsset(buyFeeAssetTx.id())
    sellFeeAssetTx = mkIssue(sender, "SELLFEE", Some(ExprScript(Terms.TRUE).explicitGet()))
    sellFeeAssetId = IssuedAsset(sellFeeAssetTx.id())
  } yield (
    sender,
    genesisTxs ++ Seq(buyFeeAssetTx, sellFeeAssetTx),
    ExchangeTransaction
      .signed(
        2.toByte,
        matcher.privateKey,
        mkOrder(sender, OrderType.BUY, matcher.publicKey, assetPair, 100, buyFeeAssetId),
        mkOrder(sender, OrderType.SELL, matcher.publicKey, assetPair, 100, sellFeeAssetId),
        100,
        5.waves,
        100,
        100,
        0.003.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet(),
    buyFeeAssetId,
    sellFeeAssetId
  )

  property("matcher fee asset script is executed during exchange transaction validation") {
    forAll(sharedParamGen2) {
      case (sender, genesisTxs, exchangeTx, buyFeeAsset, sellFeeAsset) =>
        def setAssetScript(assetId: IssuedAsset, script: Option[Script]): SetAssetScriptTransaction =
          SetAssetScriptTransaction.selfSigned(1.toByte, sender, assetId, script, 0.001.waves, ntpTime.getTimestamp()).explicitGet()

        withDomain(
          domainSettingsWithPreactivatedFeatures(
            BlockchainFeatures.SmartAccountTrading,
            BlockchainFeatures.OrderV3,
            BlockchainFeatures.SmartAssets,
            BlockchainFeatures.Ride4DApps
          )
        ) { d =>
          d.appendBlock(genesisTxs*)

          d.blockchainUpdater.processBlock(
            d.createBlock(2.toByte, Seq(setAssetScript(buyFeeAsset, Some(ExprScript(Terms.FALSE).explicitGet())), exchangeTx))
          ) should produce("TransactionNotAllowedByScript")

          d.blockchainUpdater.processBlock(
            d.createBlock(
              2.toByte,
              Seq(setAssetScript(sellFeeAsset, Some(ScriptCompiler.compile("(5 / 0) == 2", ScriptEstimatorV2).explicitGet()._1)), exchangeTx)
            )
          ) should produce("ScriptExecutionError(error = / by zero")
        }
    }
  }
}
