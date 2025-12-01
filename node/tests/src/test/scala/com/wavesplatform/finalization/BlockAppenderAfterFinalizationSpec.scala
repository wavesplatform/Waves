package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlockSpec, MessageCodec, PeerDatabase, RawBytes}
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, TestTime}
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService

import scala.jdk.CollectionConverters.*

// TODO: remove wrapBU
class BlockAppenderAfterFinalizationSpec extends BaseFinalizationSpec {
  private val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  private val testTime: TestTime                  = TestTime()

  private val seed   = ByteStr("finality-test".getBytes())
  private val sender = Wallet.generateNewAccount(seed.arr, nonce = 0)

  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 2,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )
    .copy(walletSettings = DomainPresets.DeterministicFinality.walletSettings.copy(seed = Some(seed)))

  private val generator1 = sender
  private val generator2 = Wallet.generateNewAccount(seed.arr, nonce = 1)
  private val generator3 = Wallet.generateNewAccount(seed.arr, nonce = 2)

  "should append a block" - {
    "if no one committed" in {
      withDomain(defaultSettings, AddrWithBalance.enoughBalances(generator1)) { d =>
        d.wallet.generateNewAccounts(1)

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
        d.appender.appendBlock(block)

        d.blockchain.isLastBlockId(block.id()) shouldBe true
      }
    }

    "if committed" in new BaseTest {
      override def check(d: Domain): Unit = {
        log.debug(s"Append block 3 of committed generator")
        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = committedGenerator1, strictTime = true)
        d.appender.appendBlock(block)
      }
    }.run()

    "if no one eligible committed" - {
      "all conflict" in new BaseTest {
        override def check(d: Domain): Unit = {
          log.debug(s"Append block 3 with votes")
          val block3WithVotes = d.createBlock(
            version = Block.ProtoBlockVersion,
            txs = Nil,
            generator = committedGenerator1,
            strictTime = true,
            finalizationVoting = Some(
              FinalizationVoting(
                conflict = Vector(
                  mkConflictEndorsement(committedGenerator1, committedGenerator1Idx, d.lastBlock),
                  mkConflictEndorsement(committedGenerator2, committedGenerator2Idx, d.lastBlock)
                )
              )
            )
          )
          d.appender.appendBlock(block3WithVotes)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()

      "all committed are poor" in new BaseTest {
        override def check(d: Domain): Unit = {
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
        override def check(d: Domain): Unit = {
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
            finalizationVoting = Some(
              FinalizationVoting(
                conflict = Vector(
                  mkConflictEndorsement(committedGenerator2, committedGenerator2Idx, d.lastBlock)
                )
              )
            )
          )
          d.appender.appendBlock(block3)

          log.debug(s"Append block 4 of not committed generator")
          val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = notCommittedGenerator, strictTime = true)
          d.appender.appendBlock(block)
        }
      }.run()
    }
  }

  "should reject a block" - {
    "if not committed" in {
      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        private val blsKeyPair = BlsKeyPair(generator2.privateKey)

        override def committedGenerators(at: GenerationPeriod): Vector[(Address, BlsPublicKey)] =
          Vector((generator2.toAddress, blsKeyPair.publicKey))
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(generator1, generator2),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1)

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }

    "if conflict" in {
      val validGenerator    = generator1
      val conflictGenerator = generator2
      val endorsers         = Seq(validGenerator, conflictGenerator)

      val settings = defaultSettings.configure(
        _.copy(
          generationPeriodLength = 2,
          lightNodeBlockFieldsAbsenceInterval = 0
        )
      )

      withDomain(settings, AddrWithBalance.enoughBalances(endorsers*)) { d =>
        d.wallet.generateNewAccounts(1)

        log.debug(s"Append block 2 with commitments")
        val txs                   = endorsers.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
        val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = validGenerator, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug(s"Append block 2 with endorsements")
        val otherFinalizedBlockId = TxHelpers.randomBlockId
        val block3WithVotes = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = generator1,
          strictTime = true,
          finalizationVoting = Some(
            FinalizationVoting(
              conflict = Vector(
                BlockEndorsement.signed(
                  BlsKeyPair(conflictGenerator.privateKey),
                  GeneratorIndex(1),
                  otherFinalizedBlockId,
                  finalizedHeight = GenesisBlockHeight,
                  endorsedId = block2WithCommitments.id()
                )
              )
            )
          )
        )
        d.appender.appendBlock(block3WithVotes)

        val blockFromConflict = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = conflictGenerator, strictTime = true)
        d.appender.appendBlock(blockFromConflict, requireAppended = false)

        d.blockchain.isLastBlockId(blockFromConflict.id()) shouldBe false
      }
    }
  }

  "should not broadcast a block endorsement" - {
    "before the feature activation" in {
      withDomain(DomainPresets.TransactionStateSnapshot, AddrWithBalance.enoughBalances(sender)) { d =>
        val blockChallenger = new BlockChallengerImpl(
          d.blockchain,
          new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
          d.wallet,
          d.settings,
          testTime,
          d.posSelector,
          _ => throw new RuntimeException("Unexpected call in block challenger")
        )

        val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
        val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
        val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
        channels.add(channel1)
        channels.add(channel2)
        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          Some(blockChallenger),
          d.createBlockEndorser(channels),
          appenderScheduler
        )(channel2, _, None)

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)

        testTime.setTime(block.header.timestamp)
        appender(block).runSyncUnsafe()

        val endorsements = channel1.outboundMessages().asScala.count {
          case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
          case _                                                     => false
        }

        endorsements shouldBe 0
      }
    }

    "if miner" in testWithGenerator { d =>
      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      channels.add(channel1)
      channels.add(channel2)

      def sentEndorsements: Long = {
        val r = channel1.outboundMessages().asScala.count {
          case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
          case _                                                     => false
        }
        channel1.outboundMessages().clear()
        r
      }

      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        blockChallenger = None,
        d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
        appenderScheduler
      )(channel2, _, None)

      val endorsedBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
      testTime.setTime(endorsedBlock.header.timestamp)
      appender(endorsedBlock).runSyncUnsafe()
      if (d.lastBlockId != endorsedBlock.id()) fail(s"Can't apply endorsedBlock $endorsedBlock, see logs")
      sentEndorsements shouldBe 0

      val nextBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator2, strictTime = true)
      testTime.setTime(nextBlock.header.timestamp)
      appender(nextBlock).runSyncUnsafe()
      if (d.lastBlockId != nextBlock.id()) fail(s"Can't apply nextBlock $nextBlock, see logs")
      sentEndorsements shouldBe 0
    }
  }

  "should broadcast a block endorsement if validator" in {
    val otherGenerator = Wallet.generateNewAccount(seed.arr :+ 1.toByte, nonce = 0)

    def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
      private val xs = Vector(generator1, otherGenerator).map { g =>
        (g.toAddress -> BlsKeyPair(g.privateKey).publicKey)
      }

      override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey)] = xs
    }

    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(generator1, otherGenerator),
      wrapBU = wrapBU
    ) { d =>
      d.wallet.generateNewAccounts(1)

      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      channels.add(channel1)
      channels.add(channel2)

      def sentEndorsements: Long = {
        val r = channel1.outboundMessages().asScala.count {
          case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
          case _                                                     => false
        }
        channel1.outboundMessages().clear()
        r
      }

      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        blockChallenger = None,
        d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
        appenderScheduler
      )(channel2, _, None)

      val endorsedBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
      testTime.setTime(endorsedBlock.header.timestamp)
      appender(endorsedBlock).runSyncUnsafe()
      if (d.lastBlockId != endorsedBlock.id()) fail(s"Can't apply endorsedBlock $endorsedBlock, see logs")
      sentEndorsements shouldBe 0

      val nextBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = otherGenerator, strictTime = true)
      testTime.setTime(nextBlock.header.timestamp)
      appender(nextBlock).runSyncUnsafe()
      if (d.lastBlockId != nextBlock.id()) fail(s"Can't apply nextBlock $nextBlock, see logs")
      sentEndorsements shouldBe 1
    }
  }

  // TODO: move to FinalizationSuite
  "committed generators and balances" in {
    val miner1InitBalance = 100_000.waves + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee
    val miner2InitBalance = 50_000.waves + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee

    withDomain(
      defaultSettings.configure(_.copy(generationPeriodLength = 3)),
      Seq(
        generator1 -> 10_000.waves,
        generator2 -> miner1InitBalance,
        generator3 -> miner2InitBalance
      )
    ) { d =>
      d.wallet.generateNewAccounts(3)
      d.appender.appendBlock(d.createBlock(Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))

      val generationPeriod1 = d.blockchain.generationPeriodOf(Height(1)).value.next
      generationPeriod1.start shouldBe Height(4)

      log.info("block3")
      val txs    = Seq(generator2, generator3).map(TxHelpers.commitToGeneration(generationPeriod1.start, _))
      val block3 = d.createBlock(Block.ProtoBlockVersion, txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block3)

      d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value) shouldBe empty
      d.blockchain.currentGeneratorBalances() shouldBe empty
      d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe empty

      log.info("block4, first period with committed generators")
      val transfer = TxHelpers.transfer(generator2, generator3.toAddress, amount = 4_000.waves, fee = 1_000.waves)
      val block4   = d.createBlock(Block.ProtoBlockVersion, Seq(transfer), generator = generator3, strictTime = true)
      d.appender.appendBlock(block4)

      d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod.value).map { case (addr, _) => addr } shouldBe
        Seq(generator2, generator3).map(_.toAddress)

      val miner1BalanceBeforeBlock3 = miner1InitBalance - CommitToGenerationTransaction.DepositInWavelets - TestValues.commitToGenerationFee
      val miner2BalanceBeforeBlock3 = miner2InitBalance - CommitToGenerationTransaction.DepositInWavelets - TestValues.commitToGenerationFee
      d.blockchain.currentGeneratorBalances() shouldBe Seq(
        generator2.publicKey.toAddress -> miner1BalanceBeforeBlock3,
        generator3.publicKey.toAddress -> miner2BalanceBeforeBlock3
      )
      d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe Seq(
        GeneratorEntry(generator2.toAddress, miner1BalanceBeforeBlock3, TransactionId(txs.head.id())),
        GeneratorEntry(generator3.toAddress, miner2BalanceBeforeBlock3, TransactionId(txs(1).id()))
      )

      log.info("block5")
      val block5 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator3, strictTime = true)
      d.appender.appendBlock(block5)

      val miner1BalanceBeforeBlock4 = miner1BalanceBeforeBlock3 - transfer.amount.value - transfer.fee.value
      val miner2BalanceBeforeBlock4 = miner2BalanceBeforeBlock3
      d.blockchain.currentGeneratorBalances() shouldBe Seq(
        generator2.publicKey.toAddress -> miner1BalanceBeforeBlock4,
        generator3.publicKey.toAddress -> miner2BalanceBeforeBlock4
      )
      d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe Seq(
        GeneratorEntry(generator2.toAddress, miner1BalanceBeforeBlock4, TransactionId(txs.head.id())),
        GeneratorEntry(generator3.toAddress, miner2BalanceBeforeBlock4, TransactionId(txs(1).id()))
      )
    }
  }

  private def testWithGenerator(f: Domain => Any): Any = {
    val generators = Seq(generator1, generator2)
    withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
      d.wallet.generateNewAccounts(3)

      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      (3 to 5).foreach { _ =>
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))
      }

      f(d)
    }
  }

  private trait BaseTest {
    protected val committedGenerator1     = TxHelpers.signer(0)
    protected val committedGenerator1Addr = committedGenerator1.toAddress
    protected val committedGenerator1Idx  = GeneratorIndex(0)

    protected val committedGenerator2     = TxHelpers.signer(1)
    protected val committedGenerator2Addr = committedGenerator2.toAddress
    protected val committedGenerator2Idx  = GeneratorIndex(1)

    protected val notCommittedGenerator     = TxHelpers.signer(2)
    protected val notCommittedGeneratorAddr = notCommittedGenerator.toAddress

    protected val committedGenerators = Seq(committedGenerator1, committedGenerator2)
    protected val allGenerators       = notCommittedGenerator +: committedGenerators

    def check(d: Domain): Unit

    def run(): Unit = withDomain(defaultSettings, AddrWithBalance.enoughBalances(allGenerators*)) { d =>
      log.debug(s"Append block 2 with commitments")
      val txs                   = committedGenerators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = notCommittedGenerator, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      check(d)
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
