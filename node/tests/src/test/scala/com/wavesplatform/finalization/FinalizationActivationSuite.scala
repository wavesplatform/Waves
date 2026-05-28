package com.wavesplatform.finalization

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{GeneratorIndex, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.TxHelpers

class FinalizationActivationSuite extends BaseFinalizationSpec {
  private val node0Acc = TxHelpers.signer(0)
  private val node1Acc = TxHelpers.signer(1)

  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> 5)
    .configure(_.copy(generationPeriodLength = 3))

  "activation from 5" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(node0Acc, node1Acc)) { d =>
    val genesisBlockId = d.blockchain.lastBlockId.value
    (2 to 4).foreach(_ => d.appendBlock())

    log.debug("Append block 5 with commitments")
    d.appendBlock(Seq(node0Acc, node1Acc).map(x => TxHelpers.commitToGeneration(Height(9), x))*)
    (6 to 8).foreach(_ => d.appendBlock())

    log.debug(s"Append block 9 with votes")
    d.appender.appendBlock(
      d.createBlock(
        generator = node0Acc,
        strictTime = true,
        finalizationVoting = Some(
          mkFinalizationVoting(valid = Seq(GeneratorIndex(1)))
            .signed(endorsedId = d.lastBlockId, finalizedId = genesisBlockId, validEndorsers = node1Acc)
        )
      )
    )
    d.allFinalizedHeightIs(1)

    log.debug("Append block 10")
    d.appender.appendBlock(d.createBlock(generator = node1Acc, strictTime = true))
    d.allFinalizedHeightIs(8)
  }
}
