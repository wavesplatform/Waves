package com.wavesplatform.features

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.mining.*
import com.wavesplatform.mining.MiningConstraints.MaxScriptsComplexityInBlock
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.SettingsFromDefaultConfig
import com.wavesplatform.transaction.TxHelpers

class RideV5LimitsChangeTest extends FlatSpec with WithDomain {
  "Blockchain" should "reject block with >1kk complexity before SynchronousCalls activated" in {
    val contractSigner  = TxHelpers.secondSigner
    val contractAddress = contractSigner.toAddress

    withDomain(DomainPresets.RideV4, Seq(AddrWithBalance(TxHelpers.defaultAddress), AddrWithBalance(contractAddress, 10.waves))) { d =>
      val setScript = TxHelpers.setScript(contractSigner, contract)
      d.appendBlock(setScript)

      val invokes = for (_ <- 1 to 277) yield TxHelpers.invoke(contractAddress) // 3620 complexity, 1002740 total

      val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
      val differResult = BlockDiffer.fromBlock(
        d.blockchain,
        Some(d.lastBlock),
        block,
        None,
        MiningConstraints(
          d.blockchain,
          d.blockchain.height,
          SettingsFromDefaultConfig.enableLightMode,
          Some(SettingsFromDefaultConfig.minerSettings)
        ).total,
        block.header.generationSignature
      )
      differResult should produce("Limit of txs was reached")
    }
  }

  it should "accept block with 2.5kk complexity after SynchronousCalls activated" in {
    val contractSigner  = TxHelpers.secondSigner
    val contractAddress = contractSigner.toAddress

    withDomain(DomainPresets.RideV5, Seq(AddrWithBalance(TxHelpers.defaultAddress), AddrWithBalance(contractAddress, 10.waves))) { d =>
      val setScript = TxHelpers.setScript(contractSigner, contract)
      d.appendBlock(setScript)

      val invokesCount     = 680
      val invokeComplexity = 3620
      val invokes          = for (_ <- 1 to invokesCount) yield TxHelpers.invoke(contractAddress)

      val block = d.createBlock(Block.ProtoBlockVersion, invokes, strictTime = true)
      val differResult = BlockDiffer
        .fromBlock(
          d.blockchain,
          Some(d.lastBlock),
          block,
          None,
          MiningConstraints(
            d.blockchain,
            d.blockchain.height,
            SettingsFromDefaultConfig.enableLightMode,
            Some(SettingsFromDefaultConfig.minerSettings)
          ).total,
          block.header.generationSignature
        )
        .explicitGet()
      differResult.constraint.asInstanceOf[MultiDimensionalMiningConstraint].constraints.head shouldBe OneDimensionalMiningConstraint(
        rest = MaxScriptsComplexityInBlock.AfterRideV5 - invokesCount * invokeComplexity, // 38400
        TxEstimators.scriptsComplexity,
        "MaxScriptsComplexityInBlock"
      )

      TestTime().setTime(block.header.timestamp)
      d.appendBlock(block)
      d.blockchain.height shouldBe 3
    }
  }

  private[this] val contract: Script =
    TestCompiler(V4).compileContract(
      s"""
         | {-#STDLIB_VERSION 4 #-}
         | {-#SCRIPT_TYPE ACCOUNT #-}
         | {-#CONTENT_TYPE DAPP #-}
         |
         | @Callable(tx)
         | func default() =
         |   if (${"sigVerify(base58'', base58'', base58'') ||" * 18} false) then []
         |   else []
         |
         | @Verifier(tx)
         | func verify() =
         |   ${"sigVerify(base58'', base58'', base58'') ||" * 9} true
      """.stripMargin
    )
}
