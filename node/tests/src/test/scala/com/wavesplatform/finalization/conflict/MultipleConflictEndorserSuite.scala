package com.wavesplatform.finalization.conflict

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.state.{GeneratorIndex, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.TxHelpers

class MultipleConflictEndorserSuite extends BaseFinalizationSpec {
  private val validGenerator = TxHelpers.signer(0)

  private val conflictGenerator1     = TxHelpers.signer(1)
  private val conflictGenerator1Idx  = GeneratorIndex(1)
  private val conflictGenerator2     = TxHelpers.signer(2)
  private val conflictGenerator2Addr = conflictGenerator2.toAddress
  private val conflictGenerator2Idx  = GeneratorIndex(2)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  private val generators = Seq(validGenerator, conflictGenerator1, conflictGenerator2)

  "saved conflict endorsers" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
    log.debug(s"Append block 2 with commitments")
    val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
    val block2WithCommitments = d.createBlock(txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val block3 = d.createBlock(generator = validGenerator, strictTime = true)
    log.debug(s"Append block 3")
    d.appender.appendBlock(block3)

    log.debug(s"Append microblock with conflict endorsement")
    def appendConflictEndorsements(): Unit = {
      val microBlockWithTxn = d.createMicroBlock(
        signer = Some(validGenerator),
        finalizationVoting = Some(
          mkFinalizationVoting()
            .withConflict(conflictGenerator1, conflictGenerator1Idx, block2WithCommitments.id())
            .withConflict(conflictGenerator2, conflictGenerator2Idx, block2WithCommitments.id())
        )
      )(TxHelpers.transfer(conflictGenerator1, conflictGenerator2Addr))
      d.appendMicroBlock(microBlockWithTxn)
    }
    appendConflictEndorsements()

    def checkConflictGenerators(
        at: Int = d.blockchain.height,
        expected: Set[GeneratorIndex] = Set(conflictGenerator1Idx, conflictGenerator2Idx)
    ): Unit = {
      val period = d.blockchain.generationPeriodOf(Height(at)).value
      d.blockchain.conflictGenerators(period).all shouldBe expected
    }
    checkConflictGenerators()

    log.debug("Append block 4")
    val block4Txs = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(5), x))
    val block4    = d.createBlock(block4Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block4)
    checkConflictGenerators()

    log.debug("Append block 5 of new period")
    d.appender.appendBlock(d.createBlock(generator = validGenerator, strictTime = true))
    checkConflictGenerators(expected = Set.empty)

    log.debug("Append block 6")
    d.appender.appendBlock(d.createBlock(generator = validGenerator, strictTime = true))
    checkConflictGenerators(at = 4)
    checkConflictGenerators(expected = Set.empty)

    appendConflictEndorsements()
    checkConflictGenerators()
  }
}
