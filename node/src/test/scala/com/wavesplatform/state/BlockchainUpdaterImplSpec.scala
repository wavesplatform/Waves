package com.wavesplatform.state

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.events.{BlockAppended, BlockchainUpdateTriggers, BlockchainUpdated, MicroBlockAppended, MicroBlockRollbackCompleted}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.history.{chainBaseAndMicro, randomSig}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{NTPTime, RequestGen, WithDB}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observer
import monix.reactive.subjects.ReplaySubject
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class BlockchainUpdaterImplSpec extends FreeSpec with Matchers with WithDB with RequestGen with NTPTime with DBCacheSettings {

  private val FEE_AMT = 1000000L

  // default settings, no NG
  private lazy val functionalitySettings = TestFunctionalitySettings.Stub
  private lazy val wavesSettings         = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))

  // to settings with NG enabled
  private def withNg(settings: FunctionalitySettings): FunctionalitySettings =
    settings.copy(
      blockVersion3AfterHeight = 0,
      preActivatedFeatures = settings.preActivatedFeatures ++ Map(BlockchainFeatures.NG.id -> 0)
    )
  private def withNg(settings: WavesSettings): WavesSettings =
    settings.copy(
      blockchainSettings = settings.blockchainSettings.copy(functionalitySettings = withNg(settings.blockchainSettings.functionalitySettings))
    )

  def baseTest(
      gen: Time => Gen[(KeyPair, Seq[Block])],
      enableNg: Boolean = false,
      triggers: BlockchainUpdateTriggers = BlockchainUpdateTriggers.noop
  )(
      f: (BlockchainUpdaterImpl, KeyPair) => Unit
  ): Unit = {
    val (fs, settings) =
      if (enableNg) (withNg(functionalitySettings), withNg(wavesSettings)) else (functionalitySettings, wavesSettings)

    val defaultWriter = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, fs, dbSettings)
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime, triggers)
    try {
      val (account, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      bcu.shutdown()
      f(bcu, account)
    } finally {
      bcu.shutdown()
      db.close()
    }
  }

  def createTransfer(master: KeyPair, recipient: Address, ts: Long): TransferTransaction = {
    TransferTransaction
      .selfSigned(1.toByte, master, recipient, Waves, ENOUGH_AMT / 5, Waves, 1000000, None, ts)
      .explicitGet()
  }

  def commonPreconditions(ts: Long): Gen[(KeyPair, List[Block])] = {
    for {
      master    <- accountGen
      recipient <- accountGen
      genesisBlock = TestBlock
        .create(ts, Seq(GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()))
      b1 = TestBlock
        .create(
          ts + 10,
          genesisBlock.uniqueId,
          Seq(
            createTransfer(master, recipient.toAddress, ts + 1),
            createTransfer(master, recipient.toAddress, ts + 2),
            createTransfer(recipient, master.toAddress, ts + 3),
            createTransfer(master, recipient.toAddress, ts + 4),
            createTransfer(master, recipient.toAddress, ts + 5)
          )
        )
      b2 = TestBlock.create(
        ts + 20,
        b1.uniqueId,
        Seq(
          createTransfer(master, recipient.toAddress, ts + 11),
          createTransfer(recipient, master.toAddress, ts + 12),
          createTransfer(recipient, master.toAddress, ts + 13),
          createTransfer(recipient, master.toAddress, ts + 14)
        )
      )
    } yield (master, List(genesisBlock, b1, b2))
  }

  "addressTransactions" - {
    "correctly applies transaction type filter" in {
      baseTest(time => commonPreconditions(time.correctedTime())) { (writer, account) =>
        val txs = writer
          .addressTransactions(account.toAddress, Set(GenesisTransaction.typeId), 10, None)
          .explicitGet()

        txs.length shouldBe 1
      }
    }

    "return Left if fromId argument is a non-existent transaction" in {
      baseTest(time => commonPreconditions(time.correctedTime())) { (updater, account) =>
        val nonExistentTxId = GenesisTransaction.create(account, ENOUGH_AMT, 1).explicitGet().id()

        val txs = updater
          .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), 3, Some(nonExistentTxId))

        txs shouldBe Left(s"Transaction $nonExistentTxId does not exist")
      }
    }

    "without pagination" in {
      baseTest(time => commonPreconditions(time.correctedTime())) { (updater, account) =>
        val txs = updater
          .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), 10, None)
          .explicitGet()

        val ordering = Ordering
          .by[(Int, Transaction), (Int, Long)]({ case (h, t) => (-h, -t.timestamp) })

        txs.length shouldBe 9
        txs.sorted(ordering) shouldEqual txs
      }
    }

    "with pagination" - {
      val LIMIT = 8
      def paginationTest(firstPageLength: Int): Unit = {
        baseTest(time => commonPreconditions(time.correctedTime())) { (updater, account) =>
          // using pagination
          val firstPage = updater
            .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), firstPageLength, None)
            .explicitGet()

          val rest = updater
            .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), LIMIT - firstPageLength, Some(firstPage.last._2.id()))
            .explicitGet()

          // without pagination
          val txs = updater
            .addressTransactions(account.toAddress, Set(TransferTransaction.typeId), LIMIT, None)
            .explicitGet()

          (firstPage ++ rest) shouldBe txs
        }
      }

      "after txs is in the middle of ngState" in paginationTest(3)
      "after txs is the last of ngState" in paginationTest(4)
      "after txs is in levelDb" in paginationTest(6)
    }
  }

  "blochain update events sending" - {
    sealed trait BlockchainUpdateTriggerCall
    final case class OnProcessBlock(block: Block, prevBlockchainHeight: Int, diff: DetailedDiff)            extends BlockchainUpdateTriggerCall
    final case class OnProcessMicroBlock(microBlock: MicroBlock, blockchainHeight: Int, diff: DetailedDiff) extends BlockchainUpdateTriggerCall
    final case class OnRollback(toSig: ByteStr, toHeight: Int)                                              extends BlockchainUpdateTriggerCall
    final case class OnMicroBlockRollback(toSig: ByteStr, height: Int)                                      extends BlockchainUpdateTriggerCall

    def blockchainTriggersMock: (BlockchainUpdateTriggers, Seq[BlockchainUpdateTriggerCall]) = {
      val calls = ArrayBuffer.empty[BlockchainUpdateTriggerCall]

      val t = new BlockchainUpdateTriggers {
        override def onProcessBlock(block: Block, diff: DetailedDiff, blockchainBefore: Blockchain): Unit =
          calls += OnProcessBlock(block, blockchainBefore.height, diff)

        override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain): Unit =
          calls += OnProcessMicroBlock(microBlock, blockchainBefore.height, diff)

        override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit =
          calls += OnRollback(toBlockId, toHeight)

        override def onMicroBlockRollback(toTotalResBlockSig: ByteStr, height: Int): Unit =
          calls += OnMicroBlockRollback(toTotalResBlockSig, height)
      }

      (t, calls)
    }

    "without NG" - {
      "genesis block and two transfers blocks" in {
        val (triggersMock, triggerCalls) = blockchainTriggersMock
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = false, triggersMock) { (_, _) =>
          triggerCalls.length shouldBe 3

          // genesis block
          triggerCalls.head match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 0
              block.transactionData.length shouldBe 1

              diff.parentDiff.portfolios.head._2.balance shouldBe 0
              diff.transactionDiffs.head.portfolios.head._2.balance shouldBe ENOUGH_AMT
            case _ => fail()
          }

          // transfers block
          triggerCalls(1) match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 1
              block.transactionData.length shouldBe 5

              // miner reward, no NG — all txs fees
              diff.parentDiff.portfolios.size shouldBe 1
              diff.parentDiff.portfolios.head._2.balance shouldBe FEE_AMT * 5

              // first Tx updated balances
              println(diff.transactionDiffs.head.portfolios)
              diff.transactionDiffs.head.portfolios.head._2.balance shouldBe ENOUGH_AMT / 5
              diff.transactionDiffs.head.portfolios.last._2.balance shouldBe (-ENOUGH_AMT / 5 - FEE_AMT)
            case _ => fail()
          }
        }
      }
    }

    "with NG" - {
      "genesis block and two transfers blocks" in {
        val (triggersMock, triggerCalls) = blockchainTriggersMock
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = true, triggersMock) { (_, _) =>
          triggerCalls.length shouldBe 3

          // genesis block, same as without NG
          triggerCalls.head match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 0
              block.transactionData.length shouldBe 1

              diff.parentDiff.portfolios.head._2.balance shouldBe 0
              diff.transactionDiffs.head.portfolios.head._2.balance shouldBe ENOUGH_AMT
            case _ => fail()
          }

          // first transfers block
          triggerCalls(1) match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 1
              block.transactionData.length shouldBe 5

              diff.parentDiff.portfolios.size shouldBe 1
              diff.parentDiff.portfolios.head._2.balance shouldBe FEE_AMT * 5 * 0.4
            case _ => fail()
          }

          // second transfers block, with carryFee
          triggerCalls(2) match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 2
              block.transactionData.length shouldBe 4

              diff.parentDiff.portfolios.size shouldBe 1
              diff.parentDiff.portfolios.head._2.balance shouldBe (
                FEE_AMT * 5 * 0.6     // carry from prev block
                  + FEE_AMT * 4 * 0.4 // current block reward
              )
            case _ => fail()
          }
        }
      }

      "block, then 2 microblocks, then block referencing previous microblock" in {
        def preconditions(ts: Long): Gen[(Transaction, Seq[Transaction])] =
          for {
            master    <- accountGen
            recipient <- accountGen
            genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
            transfers = Seq(
              createTransfer(master, recipient.toAddress, ts + 1),
              createTransfer(master, recipient.toAddress, ts + 2),
              createTransfer(master, recipient.toAddress, ts + 3),
              createTransfer(recipient, master.toAddress, ts + 4),
              createTransfer(master, recipient.toAddress, ts + 5)
            )
          } yield (genesis, transfers)

        val (triggersMock, triggerCalls) = blockchainTriggersMock

        val defaultWriter =
          TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, withNg(functionalitySettings), dbSettings)
        val bcu = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, withNg(wavesSettings), ntpTime, triggersMock)

        try {
          val (genesis, transfers)       = preconditions(0).sample.get
          val (block1, microBlocks1And2) = chainBaseAndMicro(randomSig, genesis, Seq(transfers.take(2), Seq(transfers(2))))
          val (block2, microBlock3)      = chainBaseAndMicro(microBlocks1And2.head.totalResBlockSig, transfers(3), Seq(Seq(transfers(4))))

          bcu.processBlock(block1).explicitGet()
          bcu.processMicroBlock(microBlocks1And2.head).explicitGet()
          bcu.processMicroBlock(microBlocks1And2.last).explicitGet()
          bcu.processBlock(block2).explicitGet() // this should remove previous microblock
          bcu.processMicroBlock(microBlock3.head).explicitGet()
          bcu.shutdown()

          triggerCalls.length shouldBe 6

          // genesis block
          triggerCalls.head match {
            case OnProcessBlock(block, prevBlockchainHeight, diff) =>
              prevBlockchainHeight shouldBe 0
              block.transactionData.length shouldBe 1

              diff.parentDiff.portfolios.head._2.balance shouldBe 0
              diff.transactionDiffs.head.portfolios.head._2.balance shouldBe ENOUGH_AMT
            case _ => fail()
          }

          // microblock 1
          triggerCalls(1) match {
            case OnProcessMicroBlock(microBlock, blockchainHeight, diff) =>
              blockchainHeight shouldBe 1
              microBlock.transactionData.length shouldBe 2
              // microBlock transactions miner reward
              diff.parentDiff.portfolios.size shouldBe 1
              diff.parentDiff.portfolios.head._2.balance shouldBe FEE_AMT * 2 * 0.4
            case _ => fail()
          }

          // microblock 2
          triggerCalls(2) match {
            case OnProcessMicroBlock(microBlock, blockchainHeight, diff) =>
              blockchainHeight shouldBe 1
              microBlock.transactionData.length shouldBe 1
              // microBlock transactions miner reward
              diff.parentDiff.portfolios.size shouldBe 1
              diff.parentDiff.portfolios.head._2.balance shouldBe FEE_AMT * 1 * 0.4
            case _ => fail()
          }

          // rollback microblock 2 before applying next keyblock
          triggerCalls(3) match {
            case OnMicroBlockRollback(toSig, height) =>
              toSig shouldBe microBlocks1And2.head.totalResBlockSig
            case _ => fail()
          }

          // next keyblock
          triggerCalls(4) match {
            case OnProcessBlock(_, prevBlockchainHeight, _) =>
              prevBlockchainHeight shouldBe 1
            case _ => fail()
          }
        } finally {
          bcu.shutdown()
          db.close()
        }
      }
    }
  }

}
