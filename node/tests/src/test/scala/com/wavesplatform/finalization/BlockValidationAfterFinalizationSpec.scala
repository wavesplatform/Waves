package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, produce}
import com.wavesplatform.transaction.TxHelpers

class BlockValidationAfterFinalizationSpec extends BaseFinalizationSpec {
  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  "should not append an invalid block" - {
    "voting for finalized block" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val finalizedHeight = Height(2)
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting(valid = Seq(committedGenerator2Idx), finalizedHeight = finalizedHeight)
            .signed(endorsedId = d.lastBlockId, finalizedId = d.blockchain.blockHeader(finalizedHeight.toInt).value.id(), committedGenerator2)
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Voting for finalized block")
      }
    }.run()

    "conflict endorsement" - {
      "duplicate" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
              .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlock.id(), GenesisBlockHeight)
              .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlock.id(), GenesisBlockHeight)
          )

          d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Duplicate conflicting endorser indexes")
        }
      }.run()

      "finalization height is greater than in voting" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
              .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlock.id(), Height(3))
          )

          d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Finalized height 3 is higher than expected 1")
        }
      }.run()

      "finalized block exists (valid endorsement among conflict)" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
              .withConflict(
                committedGenerator2,
                committedGenerator2Idx,
                endorsedId = d.lastBlockId,
                finalizedHeight = GenesisBlockHeight,
                finalizedId = d.blockchain.blockId(GenesisBlockHeight.toInt).value
              )
          )

          d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Contains expected finalized block")
        }
      }.run()
    }
  }

  private trait BaseTest {
    val committedGenerator1     = TxHelpers.signer(0)
    val committedGenerator1Addr = committedGenerator1.toAddress
    val committedGenerator1Idx  = GeneratorIndex(0)

    val committedGenerator2     = TxHelpers.signer(1)
    val committedGenerator2Addr = committedGenerator2.toAddress
    val committedGenerator2Idx  = GeneratorIndex(1)

    val notCommittedGenerator     = TxHelpers.signer(2)
    val notCommittedGeneratorAddr = notCommittedGenerator.toAddress

    val committedGenerators = Seq(committedGenerator1, committedGenerator2)
    val allGenerators       = notCommittedGenerator +: committedGenerators

    def continue(d: Domain): Unit

    def run(): Unit = withDomain(defaultSettings, AddrWithBalance.enoughBalances(allGenerators*)) { d =>
      log.debug(s"Append block 2 with commitments")
      val txs                   = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = notCommittedGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug(s"Append block 3 with votes")
      continue(d)
    }

    extension (d: Domain) {
      def createBlock(finalizationVoting: FinalizationVoting): Block = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = Nil,
        generator = committedGenerator1,
        strictTime = true,
        finalizationVoting = Some(finalizationVoting)
      )
    }
  }
}
