package com.wavesplatform.finalization

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{NumericExt, produce}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}

// Enough to check appending both blocks and microblocks, because they share the validation code
class BlockValidationAfterFinalizationSpec extends BaseFinalizationSpec {
  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  "should append a valid block" - {
    "with finalization voting" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting(valid = Seq(committedGenerator2Idx))
            .signed(endorsedId = d.lastBlockId, finalizedId = d.genesisBlockId, committedGenerator2)
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should beRight
      }
    }.run()
  }

  "should not append an invalid block" - {
    "finalization height is less than genesis height" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting(finalizedHeight = GenesisBlockHeight.prev)
            .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, Height(3))
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Finalized block height is less than 1")
      }
    }.run()

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

    "miner endorsement" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting(valid = Seq(committedGenerator1Idx))
            .signed(endorsedId = d.lastBlockId, finalizedId = d.genesisBlockId, committedGenerator1)
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Miner can't endorse its own block")
      }
    }.run()

    "with endorsement of poor generator" in new BaseTest {
      override def continue(d: Domain): Unit = {
        d.appender.appendBlock(
          d.createBlock(
            txs = Seq(
              TxHelpers.transfer(
                from = committedGenerator2,
                to = committedGenerator1Addr,
                amount = d.blockchain.balance(committedGenerator2Addr) - CommitToGenerationTransaction.DepositInWavelets - 2.waves
              )
            ),
            generator = committedGenerator1,
            strictTime = true
          )
        )

        log.debug("Append block 4 with poor generator endorsement")
        val block4 = d.createBlock(
          generator = committedGenerator1,
          strictTime = true,
          finalizationVoting = Some(
            mkFinalizationVoting(valid = Seq(committedGenerator2Idx)).signed(
              endorsedId = d.lastBlockId,
              finalizedId = d.genesisBlockId,
              validEndorsers = committedGenerator2
            )
          )
        )
        d.appender.appendBlockWithoutFallback(block4) should produce(s"Valid endorser $committedGenerator2Idx has insufficient balance")
      }
    }.run()

    "nonempty aggregated signature, but empty valid endorsers" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting()
            .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
            .signed(endorsedId = d.lastBlockId, finalizedId = d.genesisBlockId, committedGenerator3)
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce(
          "Endorsements are included, but aggregated endorsement signature is empty"
        )
      }
    }.run()

    "empty aggregated signature, but nonempty valid endorsers" in new BaseTest {
      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(mkFinalizationVoting(valid = Seq(committedGenerator2Idx)))

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce(
          "No endorsements are included, but aggregated endorsement signature is non-empty"
        )
      }
    }.run()

    "more than max valid endorsers" in new BaseTest {
      val committedGenerator4          = TxHelpers.signer(3)
      val committedGenerator5          = TxHelpers.signer(4)
      override def committedGenerators = Seq(committedGenerator1, committedGenerator2, committedGenerator3, committedGenerator4, committedGenerator5)

      override def settings: WavesSettings = super.settings.configure(_.copy(maxValidEndorsers = 1))

      override def continue(d: Domain): Unit = {
        val block3WithVotes = d.createBlock(
          mkFinalizationVoting(valid = (1 to 4).map(GeneratorIndex.apply))
            .signed(
              endorsedId = d.lastBlockId,
              finalizedId = d.genesisBlockId,
              validEndorsers = committedGenerator2,
              committedGenerator3,
              committedGenerator4,
              committedGenerator5
            )
        )

        d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Too many valid endorsements")
      }
    }.run()

    "conflicting endorsement" - {
      "duplicate" - {
        "in one block" in new BaseTest {
          override def continue(d: Domain): Unit = {
            val block3WithVotes = d.createBlock(
              mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
            )

            d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Duplicate conflicting endorser indexes")
          }
        }.run()

        "in multiple blocks of one epoch" in new BaseTest {
          override def continue(d: Domain): Unit = {
            val block3WithVote = d.createBlock(
              mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
            )
            d.appender.appendBlockWithoutFallback(block3WithVote) should beRight

            val block4WithVote = d.createBlock(
              mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
            )
            d.appender.appendBlockWithoutFallback(block4WithVote) should produce("Second conflicting endorsement from one generator")
          }
        }.run()
      }

      "and valid from same endorser" - {
        "in one block" in new BaseTest {
          override def continue(d: Domain): Unit = {
            val block3WithVotes = d.createBlock(
              mkFinalizationVoting(valid = Seq(committedGenerator2Idx), finalizedHeight = GenesisBlockHeight)
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
                .signed(d.lastBlockId, d.genesisBlockId, committedGenerator2)
            )

            d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Block contains both conflicting and valid endorsements")
          }
        }.run()

        "in multiple blocks of one epoch" in new BaseTest {
          override def continue(d: Domain): Unit = {
            d.appender.appendBlock(
              d.createBlock(
                mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
                  .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, GenesisBlockHeight)
              )
            )

            val block4 = d.createBlock(
              mkFinalizationVoting(valid = Seq(committedGenerator2Idx), finalizedHeight = GenesisBlockHeight)
                .signed(d.lastBlockId, d.genesisBlockId, committedGenerator2)
            )

            d.appender.appendBlockWithoutFallback(block4) should
              produce(s"Valid endorser $committedGenerator2Idx has insufficient balance or conflicting")
          }
        }.run()
      }

      "finalization height is greater than in voting" in new BaseTest {
        override def continue(d: Domain): Unit = {
          val block3WithVotes = d.createBlock(
            mkFinalizationVoting(finalizedHeight = GenesisBlockHeight)
              .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId, Height(3))
          )

          d.appender.appendBlockWithoutFallback(block3WithVotes) should produce("Finalized height 3 is higher than expected 1")
        }
      }.run()

      "finalized block exists (valid endorsement among conflicting)" in new BaseTest {
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

    val committedGenerator3     = TxHelpers.signer(2)
    val committedGenerator3Addr = committedGenerator3.toAddress
    val committedGenerator3Idx  = GeneratorIndex(2)

    val notCommittedGenerator     = TxHelpers.signer(9)
    val notCommittedGeneratorAddr = notCommittedGenerator.toAddress

    def committedGenerators = Seq(committedGenerator1, committedGenerator2, committedGenerator3)
    def allGenerators       = notCommittedGenerator +: committedGenerators

    def settings: WavesSettings = defaultSettings

    def continue(d: Domain): Unit

    def run(): Unit = withDomain(settings, AddrWithBalance.enoughBalances(allGenerators*)) { d =>
      log.debug(s"Append block 2 with commitments")
      val txs                   = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = notCommittedGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug(s"Append block 3")
      continue(d)
    }

    extension (d: Domain) {
      def genesisBlockId: BlockId = d.blockchain.blockHeader(GenesisBlockHeight.toInt).value.id()

      def createBlock(finalizationVoting: FinalizationVoting): Block =
        d.createBlock(generator = committedGenerator1, strictTime = true, finalizationVoting = Some(finalizationVoting))
    }
  }
}
