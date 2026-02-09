package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, produce}
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

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator, strictTime = true)
        d.appender.appendBlock(block)
      }
    }

    "if committed" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 of committed generator")
        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block)
      }
    }.run()

    "if no one eligible committed" - {
      "all conflict" in pendingUntilFixed(new BaseTest {
        override def continue(d: Domain): Unit = {
          log.debug(s"Append block 3 with votes")
          val block3WithVotes = d.createBlock(
            version = Block.ProtoBlockVersion,
            txs = Nil,
            generator = committedGenerator1,
            strictTime = true,
            finalizationVoting = Some(
              mkFinalizationVoting()
                .withConflict(committedGenerator1, committedGenerator1Idx, d.lastBlock.id())
                .withConflict(committedGenerator2, committedGenerator2Idx, d.lastBlock.id())
            )
          )
          d.appender.appendBlock(block3WithVotes)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run())

      "all committed are poor" in new BaseTest {
        override def continue(d: Domain): Unit = {
          log.debug(s"Append block 3 with spending")
          val block3WithSpending = d.createBlock(
            version = Block.ProtoBlockVersion,
            txs = Seq(committedGenerator1, committedGenerator2).map { kp =>
              TxHelpers.transfer(kp, notCommittedGeneratorAddr, amount = d.balance(kp.toAddress) - TestValues.fee - DepositInWavelets)
            },
            generator = committedGenerator1,
            strictTime = true
          )
          d.appender.appendBlock(block3WithSpending)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()

      "poor conflict, rest conflict" in new BaseTest {
        override def continue(d: Domain): Unit = {
          log.debug(s"Append block 3 with vote and spending")
          val block3 = d.createBlock(
            version = Block.ProtoBlockVersion,
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
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()
    }

    "on new period if was conflict on previous" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 with votes")
        val block3WithVotes = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = committedGenerator2,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator1, committedGenerator1Idx, d.lastBlock.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug(s"Append empty blocks")
        (4 to 5).foreach { _ =>
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = committedGenerator2, strictTime = true)
          d.appender.appendBlock(block)
        }

        log.debug(s"Append new period block")
        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block)
      }
    }.run()
  }

  "should reject a block" - {
    "if not committed" in new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 of not committed generator")
        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }.run()

    "if conflict" in pendingUntilFixed(new BaseTest {
      override def continue(d: Domain): Unit = {
        log.debug(s"Append block 3 with votes")
        val block3WithVotes = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = committedGenerator1,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(committedGenerator1, committedGenerator1Idx, d.lastBlock.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug(s"Append block 4")
        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }.run())
  }

  "can't append block without microblocks with spending transactions" in withDomain(
    defaultSettings,
    AddrWithBalance.enoughBalances(committedGenerator1, notCommittedGenerator)
  ) { d =>
    log.debug(s"Append block 2 with commitments")
    val block2 = d.createBlock(
      version = Block.ProtoBlockVersion,
      txs = Seq(TxHelpers.commitToGeneration(Height(3), committedGenerator1)),
      generator = committedGenerator1
    )
    d.appendBlock(block2)

    log.debug(s"Append key block 3")
    d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = committedGenerator1, strictTime = true))
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
      d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, ref = Some(keyBlockId), generator = notCommittedGenerator, strictTime = true)
    ) should produce("is not allowed to generate a block")
  }

  private trait BaseTest {
    protected val committedGenerators = Seq(committedGenerator1, committedGenerator2)
    protected val allGenerators       = notCommittedGenerator +: committedGenerators

    def continue(d: Domain): Unit

    def run(): Unit = withDomain(defaultSettings, AddrWithBalance.enoughBalances(allGenerators*)) { d =>
      log.debug(s"Append block 2 with commitments")
      val txs                   = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = notCommittedGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      continue(d)
    }
  }
}
