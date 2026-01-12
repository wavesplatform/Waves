package com.wavesplatform.finalization.conflict

import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.state.{GeneratorIndex, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.produce
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position

class MultipleMicroBlocksWithConflictEndorserSuite extends BaseFinalizationSpec {
  private val validGenerator     = TxHelpers.signer(0)
  private val validGeneratorAddr = validGenerator.toAddress

  private val conflictGenerator    = TxHelpers.signer(1)
  private val conflictGeneratorIdx = GeneratorIndex(1)

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings.configure(
    _.copy(
      generationPeriodLength = 2,
      lightNodeBlockFieldsAbsenceInterval = 0
    )
  )

  private val generators = Seq(validGenerator, conflictGenerator)

  "second microblock appended" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
    log.debug(s"Append block 2 with commitments")
    val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
    val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val block3 = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true)
    log.debug(s"Append block 3")
    d.appender.appendBlock(block3)

    log.debug(s"Append microblock with conflict endorsement")
    val microBlockWithTxn1 = d.createMicroBlock(
      signer = Some(validGenerator),
      finalizationVoting = Some(mkFinalizationVoting().withConflict(conflictGenerator, conflictGeneratorIdx, block2WithCommitments.id()))
    )(TxHelpers.transfer(conflictGenerator, validGeneratorAddr))
    d.appendMicroBlock(microBlockWithTxn1)

    log.debug(s"Can't append microblock with same conflict endorsement")
    val microBlockWithTxn2 = d.createMicroBlock(
      signer = Some(validGenerator),
      finalizationVoting = Some(mkFinalizationVoting().withConflict(conflictGenerator, conflictGeneratorIdx, block2WithCommitments.id()))
    )(TxHelpers.transfer(conflictGenerator, validGeneratorAddr))
    d.appendMicroBlockE(microBlockWithTxn2) should produce("Duplicate conflicting endorser indexes")

    log.debug(s"Append microblock without endorsements")
    d.appendMicroBlockE(TxHelpers.transfer(conflictGenerator, validGeneratorAddr)) should beRight
  }
}
