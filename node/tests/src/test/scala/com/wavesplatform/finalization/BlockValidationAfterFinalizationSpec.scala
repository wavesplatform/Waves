package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.FreeSpec
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
    "conflict endorsement" - {
      "finalization height is greater than in voting" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(
              finalizedHeight = Height(1),
              conflict = Seq(mkConflictEndorsement(committedGenerator2, committedGenerator2Idx, d.lastBlock.id(), Height(3)))
            )
          )

          d.appender.appendBlock(block3WithVotes, requireAppended = false)
          d.blockchain.height shouldBe 2
        }
      }.run()

      "finalized block exists (valid endorsement among conflict)" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(
              finalizedHeight = GenesisBlockHeight,
              conflict = Seq(
                mkConflictEndorsement(
                  committedGenerator2,
                  committedGenerator2Idx,
                  endorsedId = d.lastBlockId,
                  finalizedHeight = GenesisBlockHeight,
                  finalizedId = d.blockchain.blockId(GenesisBlockHeight.toInt).value
                )
              )
            )
          )

          d.appender.appendBlock(block3WithVotes, requireAppended = false)
          d.blockchain.height shouldBe 2
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
