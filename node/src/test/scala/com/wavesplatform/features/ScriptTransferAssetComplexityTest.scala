package com.wavesplatform.features

import cats.implicits._
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.it.util._
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp => DAppType, _}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.mining.{MiningConstraints, MultiDimensionalMiningConstraint, OneDimensionalMiningConstraint}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers}

class ScriptTransferAssetComplexityTest extends FlatSpec with Matchers with WithDomain with PathMockFactory {
  "InvokeScript differ" should "count assets verifier complexity" in withDomain(
    domainSettingsWithFeatures(
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.MassTransfer
    )
  ) { d =>
    val contractSigner  = TxHelpers.secondSigner
    val contractAddress = contractSigner.toAddress
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress), TxHelpers.genesis(contractAddress, 10 waves))

    val issue         = TxHelpers.issue(script = assetScript)
    val setScript     = TxHelpers.setScript(contractSigner, contract(issue.asset))
    val transferAsset = TxHelpers.transfer(to = contractAddress, amount = 100, asset = issue.asset)
    d.appendBlock(setScript, issue, transferAsset)

    val invokes = for (_ <- 1 to 1) yield TxHelpers.invoke(contractAddress, "test") // Complexity = 82 without asset script

    val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
    val differResult = BlockDiffer
      .fromBlock(
        d.blockchain,
        Some(d.lastBlock),
        block,
        MiningConstraints(d.blockchain, d.blockchain.height, Some(defaultDomainSettings.minerSettings)).total
      )
      .explicitGet()

    val rest =
      differResult.constraint.asInstanceOf[MultiDimensionalMiningConstraint].constraints.head.asInstanceOf[OneDimensionalMiningConstraint].rest
    val burntComplexity = 1000000 - rest - 82
    println(s"Burnt complexity is $burntComplexity")
    burntComplexity shouldBe 18370
  }

  it should "count assets verifier complexity with RideV5" in withDomain(
    domainSettingsWithFeatures(
      BlockchainFeatures.Ride4DApps,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.MassTransfer,
      BlockchainFeatures.SynchronousCalls
    )
  ) { d =>
    val contractSigner  = TxHelpers.secondSigner
    val contractAddress = contractSigner.toAddress
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress), TxHelpers.genesis(contractAddress, 10 waves))

    val issue         = TxHelpers.issue(script = assetScript)
    val setScript     = TxHelpers.setScript(contractSigner, contract(issue.asset))
    val transferAsset = TxHelpers.transfer(to = contractAddress, amount = 100, asset = issue.asset)
    d.appendBlock(setScript, issue, transferAsset)

    val invokes = for (_ <- 1 to 1) yield TxHelpers.invoke(contractAddress, "test") // Complexity = 82 without asset script

    val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
    val differResult = BlockDiffer
      .fromBlock(
        d.blockchain,
        Some(d.lastBlock),
        block,
        MiningConstraints(d.blockchain, d.blockchain.height, Some(defaultDomainSettings.minerSettings)).total
      )
      .explicitGet()

    val rest =
      differResult.constraint.asInstanceOf[MultiDimensionalMiningConstraint].constraints.head.asInstanceOf[OneDimensionalMiningConstraint].rest
    val burntComplexity = 2500000 - rest - 82
    println(s"Burnt complexity is $burntComplexity")
    burntComplexity shouldBe 18370
  }

  private[this] def contract(asset: IssuedAsset): Script = {
    val ctx = {
      val directives = DirectiveSet(V4, Account, DAppType).explicitGet()
      PureContext.build(V4).withEnvironment[Environment] |+|
        CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
        WavesContext.build(Global, directives)
    }

    val script =
      s"""
        | {-#STDLIB_VERSION 4 #-}
        | {-#SCRIPT_TYPE ACCOUNT #-}
        | {-#CONTENT_TYPE DAPP #-}
        |
        | @Callable(tx)
        | func test() =
        |   [${(1 to 10).map(_ => s"ScriptTransfer(tx.caller, 1, base58'$asset')").mkString(",\n")}]
      """.stripMargin

    val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
    ContractScript(V4, dApp).explicitGet()
  }

  private[this] val assetScript: Script = {
    val ctx = {
      val directives = DirectiveSet(V4, Asset, Expression).explicitGet()
      PureContext.build(V4).withEnvironment[Environment] |+|
        CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
        WavesContext.build(Global, directives)
    }

    val script =
      s"""
         |{-#STDLIB_VERSION 4 #-}
         |{-#SCRIPT_TYPE ASSET #-}
         |{-#CONTENT_TYPE EXPRESSION #-}
         |
         |${"sigVerify(base58'', base58'', base58'') ||" * 9} true
      """.stripMargin

    val compiled        = ExpressionCompiler.compileBoolean(script, ctx.compilerContext).explicitGet()
    val assetComplexity = ExprScript.estimateExact(compiled, V4, ScriptEstimatorV3).explicitGet()
    println(s"Asset script complexity is $assetComplexity")
    ExprScript(V4, compiled).explicitGet()
  }
}
