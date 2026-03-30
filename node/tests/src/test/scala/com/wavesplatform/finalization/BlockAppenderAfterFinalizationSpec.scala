package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{NumericExt, produce}
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}

class BlockAppenderAfterFinalizationSpec extends BaseFinalizationSpec {
  protected val committedGenerator1     = TxHelpers.signer(0)
  protected val committedGenerator1Addr = committedGenerator1.toAddress
  protected val committedGenerator1Idx  = GeneratorIndex(0)

  protected val committedGenerator2     = TxHelpers.signer(1)
  protected val committedGenerator2Addr = committedGenerator2.toAddress
  protected val committedGenerator2Idx  = GeneratorIndex(1)

  protected val notCommittedGenerator     = TxHelpers.signer(2)
  protected val notCommittedGeneratorAddr = notCommittedGenerator.toAddress

  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  "should append a block" - {
    "if no one committed" in {
      val generator = TxHelpers.signer(0)
      withDomain(defaultSettings, AddrWithBalance.enoughBalances(generator)) { d =>
        d.wallet.generateNewAccounts(1)

        val block = d.createBlock(generator = generator, strictTime = true)
        d.appender.appendBlock(block)
      }
    }

    "if committed" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 of committed generator")
        val block = d.createBlock(generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block)
      }
    }.run()

    "if no one eligible committed" - {
      "all committed are poor" in new BaseTest {
        override def continue(d: Domain): Unit = {
          log.debug(s"Append block 3 with spending")
          val block3WithSpending = d.createBlock(
            txs = Seq(committedGenerator1, committedGenerator2).map { kp =>
              TxHelpers.transfer(kp, notCommittedGeneratorAddr, amount = d.balance(kp.toAddress) - TestValues.fee - DepositInWavelets)
            },
            generator = committedGenerator1,
            strictTime = true
          )
          d.appender.appendBlock(block3WithSpending)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()

      "poor conflict, rest conflict" in new BaseTest {
        override def continue(d: Domain): Unit = {
          log.debug(s"Append block 3 with vote and spending")
          val block3 = d.createBlock(
            txs = Seq(
              TxHelpers.transfer(
                committedGenerator1,
                notCommittedGeneratorAddr,
                amount = d.balance(committedGenerator1Addr) - TestValues.fee - DepositInWavelets
              )
            ),
            generator = committedGenerator1,
            strictTime = true,
            finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlock.id()))
          )
          d.appender.appendBlock(block3)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()
    }

    "on new period if was conflict on previous" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 with votes")
        val block3WithVotes = d.createBlock(
          generator = committedGenerator2,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator1, committedGenerator1Idx, d.lastBlock.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug(s"Append empty blocks")
        (4 to 5).foreach { _ =>
          val block = d.createBlock(generator = committedGenerator2, strictTime = true)
          d.appender.appendBlock(block)
        }

        log.debug(s"Append new period block")
        val block = d.createBlock(generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block)
      }
    }.run()

    "if sent conflicting endorsement in the last microblock, that removed" - {
      "can append a keyblock referencing keyblock" in {
        val committedGenerators = Seq(committedGenerator1, committedGenerator2)

        withDomain(
          defaultSettings.configure(_.copy(generationPeriodLength = 3)),
          AddrWithBalance.enoughBalances(committedGenerators*)
        ) { d =>
          log.debug(s"Append block 2 with commitments")
          val txs = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x))
          d.appender.appendBlock(d.createBlock(txs, generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 3")
          d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 4 of new epoch with conflicting endorsement in the last microblock")
          val block4 = d.createBlock(generator = committedGenerator1, strictTime = true)
          d.appender.appendBlock(block4)
          d.appendMicroBlock(
            d.createMicroBlock(
              signer = Some(committedGenerator1),
              finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId))
            )(TxHelpers.transfer(committedGenerator1))
          )

          log.debug(s"Append block 5 of conflicting generator")
          d.appender.appendBlock(
            d.createBlock(ref = Some(block4.id()), generator = committedGenerator2, strictTime = true)
          )

          withClue("Not finalized: ") {
            d.finalizedHeightAtPrevIs(1)
            d.finalizedHeightIs(1)
          }
        }
      }

      "can append a keyblock referencing microblock" in {
        val committedGenerators = Seq(committedGenerator1, committedGenerator2)

        withDomain(
          defaultSettings.configure(_.copy(generationPeriodLength = 3)),
          AddrWithBalance.enoughBalances(committedGenerators*)
        ) { d =>
          log.debug(s"Append block 2 with commitments")
          val txs = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x))
          d.appender.appendBlock(d.createBlock(txs, generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 3")
          d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 4 of new epoch with conflicting endorsement in the last microblock")
          d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))
          val block4 = d.appendMicroBlock(d.createMicroBlock(signer = Some(committedGenerator1))(TxHelpers.transfer(committedGenerator1)))
          d.appendMicroBlock(
            d.createMicroBlock(
              signer = Some(committedGenerator1),
              finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId))
            )(TxHelpers.transfer(committedGenerator1))
          )

          log.debug(s"Append block 5 of conflicting generator")
          d.appender.appendBlock(
            d.createBlock(ref = Some(block4), generator = committedGenerator2, strictTime = true)
          )

          withClue("Not finalized: ") {
            d.finalizedHeightAtPrevIs(1)
            d.finalizedHeightIs(1)
          }
        }
      }

      "referencing microblock and forging by another" in {
        val committedGenerators = Seq(committedGenerator1, committedGenerator2)

        withDomain(
          defaultSettings.configure(_.copy(generationPeriodLength = 3)),
          AddrWithBalance.enoughBalances(committedGenerators*)
        ) { d =>
          log.debug(s"Append block 2 with commitments")
          val txs = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x))
          d.appender.appendBlock(d.createBlock(txs, generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 3")
          d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))

          log.debug(s"Append block 4 of new epoch with conflicting endorsement in the last microblock")
          d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))
          val parentBlockId = d.appendMicroBlock(d.createMicroBlock(signer = Some(committedGenerator1))(TxHelpers.transfer(committedGenerator1)))
          d.appendMicroBlock(
            d.createMicroBlock( // Finalization reached
              signer = Some(committedGenerator1),
              finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlockId))
            )(TxHelpers.transfer(committedGenerator1))
          )

          log.debug(s"Append block 5 of valid generator")
          d.appender.appendBlock( // Finalization reset
            d.createBlock(ref = Some(parentBlockId), generator = committedGenerator1, strictTime = true)
          )

          withClue("Not finalized: ") {
            d.finalizedHeightAtPrevIs(1)
            d.finalizedHeightIs(1) // Because we need committedGenerator2 balance for finalization
          }
        }
      }
    }

    "if sent LeaseCancel in the last microblock, that removed" in {
      val committedGenerators = Seq(committedGenerator1, committedGenerator2)
      withDomain(
        defaultSettings.configure(
          _.copy(
            generationPeriodLength = 51,
            generationBalanceDepthFrom50To1000AfterHeight = 1000
          )
        ),
        AddrWithBalance.enoughBalances(committedGenerator1) :+ AddrWithBalance(
          committedGenerator2Addr,
          CommitToGenerationTransaction.DepositInWavelets + 1.waves
        )
      ) { d =>
        log.debug(s"Append block 2 with leasing")
        val leasingTxn = TxHelpers.lease(committedGenerator1, committedGenerator2Addr, amount = 20_000.waves)
        d.appender.appendBlock(
          d.createBlock(
            txs = Seq(leasingTxn),
            generator = committedGenerator1,
            strictTime = true
          )
        )

        log.debug("Appending [3; 51] blocks")
        (3 to 50).foreach { _ =>
          d.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))
        }
        d.appender.appendBlock(
          d.createBlock(generator = committedGenerator1, strictTime = true)
        )

        log.debug("Commit to generation")
        d.appendMicroBlock(
          d.createMicroBlock(signer = Some(committedGenerator1))(
            committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(52), x))*
          )
        )
        val block51Id = d.lastBlockId

        log.debug("Cancel leasing for committedGenerator2 in microblock")
        d.appendMicroBlock(d.createMicroBlock(signer = Some(committedGenerator1))(TxHelpers.leaseCancel(leasingTxn.id(), committedGenerator1)))

        log.debug(s"Append block 52 referencing keyblock")
        d.appender.appendBlock(
          d.createBlock(ref = Some(block51Id), generator = committedGenerator2, strictTime = true)
        )
      }
    }
  }

  "should reject a block of generator" - {
    "if not committed" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 of not committed generator")
        val block = d.createBlock(generator = notCommittedGenerator, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }.run()

    "if committed in the last microblock, that removed" in {
      val committedGenerators = Seq(committedGenerator1, committedGenerator2)
      val allGenerators       = notCommittedGenerator +: committedGenerators

      withDomain(
        defaultSettings.configure(_.copy(generationPeriodLength = 2)),
        AddrWithBalance.enoughBalances(allGenerators*)
      ) { d =>
        log.debug(s"Append block 2 with commitments")
        val txs    = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
        val block2 = d.createBlock(txs, generator = notCommittedGenerator, strictTime = true)
        d.appender.appendBlock(block2)
        d.appendMicroBlock(
          d.createMicroBlock(
            signer = Some(notCommittedGenerator)
          )(TxHelpers.commitToGeneration(generationPeriodStart = Height(3), notCommittedGenerator))
        )

        log.debug(s"Append block 3 of not committed generator")
        val newBlock =
          d.createBlock(ref = Some(block2.id()), generator = notCommittedGenerator, strictTime = true)
        d.appender.appendBlock(newBlock, requireAppended = false)
        d.blockchain.isLastBlockId(newBlock.id()) shouldBe false
      }
    }

    "if conflict" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 with votes")
        val block3WithVotes = d.createBlock(
          generator = committedGenerator2,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator1, committedGenerator1Idx, d.lastBlock.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug(s"Append block 4")
        val block = d.createBlock(generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }.run()

    "spent all WAVES" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(committedGenerator1)
    ) { d =>
      log.debug(s"Append block 2 with commitments")
      val block2 = d.createBlock(
        Seq(TxHelpers.commitToGeneration(Height(3), committedGenerator1)),
        generator = committedGenerator1
      )
      d.appendBlock(block2)

      log.debug(s"Append key block 3")
      d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))

      log.debug(s"Append micro block with spending")
      d.appendMicroBlock(
        d.createMicroBlock(signer = Some(committedGenerator1))(
          TxHelpers.transfer(
            from = committedGenerator1,
            to = notCommittedGeneratorAddr,
            amount = d.blockchain.balance(committedGenerator1Addr) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
            fee = 1.waves
          )
        )
      )

      log.debug("Append block 4")
      d.appender.appendBlockWithoutFallback(
        d.createBlock(generator = committedGenerator1, strictTime = true)
      ) should produce("less than required for generation")
    }

    "that not committed, if generator set is empty in the removed micro block" in withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(committedGenerator1, notCommittedGenerator)
    ) { d =>
      log.debug(s"Append block 2 with commitments")
      val block2 = d.createBlock(
        Seq(TxHelpers.commitToGeneration(Height(3), committedGenerator1)),
        generator = committedGenerator1
      )
      d.appendBlock(block2)

      log.debug(s"Append key block 3")
      d.appender.appendBlock(d.createBlock(generator = committedGenerator1, strictTime = true))
      val keyBlockId = d.lastBlockId

      log.debug(s"Append micro block with spending")
      d.appendMicroBlock(
        d.createMicroBlock(signer = Some(committedGenerator1))(
          TxHelpers.transfer(
            from = committedGenerator1,
            to = notCommittedGeneratorAddr,
            amount = d.blockchain.balance(committedGenerator1Addr) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
            fee = 1.waves
          )
        )
      )

      log.debug("Append block 4")
      d.appender.appendBlockWithoutFallback(
        d.createBlock(ref = Some(keyBlockId), generator = notCommittedGenerator, strictTime = true)
      ) should produce("is not allowed to generate a block")
    }
  }

  private trait BaseTest {
    protected val committedGenerators = Seq(committedGenerator1, committedGenerator2)
    protected val allGenerators       = notCommittedGenerator +: committedGenerators

    def continue(d: Domain): Unit

    def run(): Unit = withDomain(defaultSettings, AddrWithBalance.enoughBalances(allGenerators*)) { d =>
      log.debug(s"Append block 2 with commitments")
      val txs                   = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = notCommittedGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      continue(d)
    }
  }
}
