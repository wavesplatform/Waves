package com.wavesplatform.features

import cats.implicits._
import com.wavesplatform.TestTime
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.diffs.ProduceError.produce
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.it.util._
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp => DAppType, _}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.mining.{MiningConstraints, MultiDimensionalMiningConstraint, OneDimensionalMiningConstraint, TxEstimators}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.transaction.TxHelpers
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers}

class RideV5LimitsChangeTest extends FlatSpec with Matchers with WithDomain with PathMockFactory {
  "Blockchain" should "reject block with >1kk complexity before SynchronousCalls activated" in withDomain(
    domainSettingsWithFeatures(BlockchainFeatures.Ride4DApps, BlockchainFeatures.BlockV5, BlockchainFeatures.MassTransfer)
  ) { d =>
    val contractSigner  = TxHelpers.secondSigner
    val contractAddress = contractSigner.toAddress
    d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress), TxHelpers.genesis(contractAddress, 10 waves))

    val setScript = TxHelpers.setScript(contractSigner, contract)
    d.appendBlock(setScript)

    val invokes = for (_ <- 1 to 273) yield TxHelpers.invoke(contractAddress, "test") // 3675 complexity, 1003275 total

    val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
    val differResult = BlockDiffer.fromBlock(
      d.blockchain,
      Some(d.lastBlock),
      block,
      MiningConstraints(d.blockchain, d.blockchain.height, Some(SettingsFromDefaultConfig.minerSettings)).total
    )
    differResult should produce("Limit of txs was reached")
  }

  it should "accept block with 2.5kk complexity after SynchronousCalls activated" in withDomain(
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

    val setScript = TxHelpers.setScript(contractSigner, contract)
    d.appendBlock(setScript)

    val invokes = for (_ <- 1 to 680) yield TxHelpers.invoke(contractAddress, "test") // 3675 complexity, 2499000 total

    val time       = new TestTime()

    val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
    val differResult = BlockDiffer
      .fromBlock(
        d.blockchain,
        Some(d.lastBlock),
        block,
        MiningConstraints(d.blockchain, d.blockchain.height, Some(SettingsFromDefaultConfig.minerSettings)).total
      )
      .explicitGet()
    differResult.constraint.asInstanceOf[MultiDimensionalMiningConstraint].constraints.head shouldBe OneDimensionalMiningConstraint(
      rest = 1000,
      TxEstimators.scriptsComplexity,
      "MaxScriptsComplexityInBlock"
    )

    time.setTime(block.header.timestamp)
    d.appendBlock(block)
    d.blockchain.height shouldBe 3
  }

  private[this] val contract: Script = {
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
        |   if (${"sigVerify(base58'', base58'', base58'') ||" * 18} false) then []
        |   else []
        |
        | @Verifier(tx)
        | func verify() =
        |   ${"sigVerify(base58'', base58'', base58'') ||" * 9} true
      """.stripMargin

    val dApp = ContractCompiler.compile(script, ctx.compilerContext, V4).explicitGet()
    ContractScript(V4, dApp).explicitGet()
  }
}
