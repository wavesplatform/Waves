package com.wavesplatform.finalization.conflict

import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.finalization.BaseFinalizationSpec
import com.wavesplatform.state.{GeneratorIndex, GenesisBlockHeight, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.TxHelpers
import org.scalactic.source.Position

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

  private val endorsers = Seq(validGenerator, conflictGenerator1, conflictGenerator2)

  "saved conflict endorsers" in withDomain(defaultSettings, AddrWithBalance.enoughBalances(endorsers*)) { d =>
    log.debug(s"Append block 2 with commitments")
    val txs                   = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
    val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block2WithCommitments)

    val block3 = d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true)
    log.debug(s"Append block 3")
    d.appender.appendBlock(block3)

    log.debug(s"Append microblock with conflict endorsement")
    def appendConflictEndorsements(): Unit = {
      val otherFinalizedBlockId = TxHelpers.randomBlockId
      val microBlockWithTxn = d.createMicroBlock(
        signer = Some(validGenerator),
        finalizationVoting = Some(
          FinalizationVoting(
            conflict = Vector(
              BlockEndorsement.signed(
                BlsKeyPair(conflictGenerator1.privateKey),
                conflictGenerator1Idx,
                otherFinalizedBlockId,
                finalizedHeight = GenesisBlockHeight,
                endorsedId = block2WithCommitments.id()
              ),
              BlockEndorsement.signed(
                BlsKeyPair(conflictGenerator2.privateKey),
                conflictGenerator2Idx,
                otherFinalizedBlockId,
                finalizedHeight = GenesisBlockHeight,
                endorsedId = block2WithCommitments.id()
              )
            )
          )
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
    val block4Txs = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(5), x))
    val block4    = d.createBlock(version = Block.ProtoBlockVersion, txs = block4Txs, generator = validGenerator, strictTime = true)
    d.appender.appendBlock(block4)
    checkConflictGenerators()

    log.debug("Append block 5 of new epoch")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true))
    checkConflictGenerators(expected = Set.empty)

    log.debug("Append block 6")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = validGenerator, strictTime = true))
    checkConflictGenerators(at = 4)
    checkConflictGenerators(expected = Set.empty)

    appendConflictEndorsements()
    checkConflictGenerators()
  }
}
