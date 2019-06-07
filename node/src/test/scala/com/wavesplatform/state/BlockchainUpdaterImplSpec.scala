package com.wavesplatform.state

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{chainBaseAndMicro, randomSig}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionV1}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{NTPTime, RequestGen, WithDB}
import monix.reactive.Observer
import monix.reactive.subjects.ReplaySubject
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

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
      blockchainSettings = settings.blockchainSettings.copy(functionalitySettings = withNg(settings.blockchainSettings.functionalitySettings)))

  private def withBlockchainUpdatesEnabled(settings: WavesSettings): WavesSettings =
    settings.copy(enableBlockchainUpdates = true)

  def baseTest(gen: Time => Gen[(KeyPair, Seq[Block])], enableNg: Boolean = false, events: Observer[BlockchainUpdated] = Observer.empty)(
      f: (BlockchainUpdaterImpl, KeyPair) => Unit): Unit = {
    val (fs, settings) =
      if (enableNg) (withNg(functionalitySettings), withNg(wavesSettings)) else (functionalitySettings, wavesSettings)

    val defaultWriter = new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, dbSettings)
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, withBlockchainUpdatesEnabled(settings), ntpTime, events)
    try {
      val (account, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      events.onComplete()
      bcu.shutdown()
      f(bcu, account)
    } finally {
      events.onComplete()
      bcu.shutdown()
      db.close()
    }
  }

  def createTransfer(master: KeyPair, recipient: Address, ts: Long): TransferTransaction = {
    TransferTransactionV1
      .selfSigned(Waves, master, recipient, ENOUGH_AMT / 5, ts, Waves, 1000000, Array.emptyByteArray)
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
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 3, Some(nonExistentTxId))

        txs shouldBe Left(s"Transaction $nonExistentTxId does not exist")
      }
    }

    "without pagination" in {
      baseTest(time => commonPreconditions(time.correctedTime())) { (updater, account) =>
        val txs = updater
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 10, None)
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
            .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), firstPageLength, None)
            .explicitGet()

          val rest = updater
            .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), LIMIT - firstPageLength, Some(firstPage.last._2.id()))
            .explicitGet()

          // without pagination
          val txs = updater
            .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), LIMIT, None)
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
    "without NG" - {
      "genesis block and two transfers blocks" in {
        val events = ReplaySubject[BlockchainUpdated]()
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = false, events) { (_, _) =>
          val updates = events.toListL
            .runSyncUnsafe(5.seconds)

          updates.length shouldBe 3

          // genesis block
          updates.head match {
            case BlockAppended(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 1
              block.transactionData.length shouldBe 1
              blockStateUpdate.balances.length shouldBe 0
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT
            case _ => fail()
          }

          // transfers block
          updates(1) match {
            case BlockAppended(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 2
              block.transactionData.length shouldBe 5
              // miner reward, no NG — all txs fees
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe FEE_AMT * 5

              // first Tx updated balances
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT / 5
              transactionsStateUpdates.head.balances.last._3 shouldBe (ENOUGH_AMT - ENOUGH_AMT / 5 - FEE_AMT)
            case _ => fail()
          }
        }
      }
    }

    "with NG" - {
      "genesis block and two transfers blocks" in {
        val events = ReplaySubject[BlockchainUpdated]()
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = true, events) { (_, _) =>
          val updates = events.toListL
            .runSyncUnsafe(5.seconds)

          updates.length shouldBe 3

          // genesis block, same as without NG
          updates.head match {
            case BlockAppended(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 1
              block.transactionData.length shouldBe 1
              blockStateUpdate.balances.length shouldBe 0
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT
            case _ => fail()
          }

          // first transfers block
          updates(1) match {
            case BlockAppended(block, height, blockStateUpdate, _) =>
              height shouldBe 2
              block.transactionData.length shouldBe 5

              // miner reward, with NG — 40% of all txs fees
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe FEE_AMT * 5 * 0.4
            case _ => fail()
          }

          // second transfers block, with carryFee
          updates(2) match {
            case BlockAppended(block, height, blockStateUpdate, _) =>
              height shouldBe 3
              block.transactionData.length shouldBe 4

              // miner reward, with NG — 40% of all txs fees and 60% from previous block
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe (
                FEE_AMT * 5 * 0.4     // miner balance from prev block
                  + FEE_AMT * 4 * 0.4 // carry from prev block
                  + FEE_AMT * 5 * 0.6 // current block reward
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
              createTransfer(master, recipient.toAddress, ts + 5),
            )
          } yield (genesis, transfers)

        val events = ReplaySubject[BlockchainUpdated]()
        val defaultWriter =
          new LevelDBWriter(db, ignoreSpendableBalanceChanged, withNg(functionalitySettings), dbSettings)
        val bcu = new BlockchainUpdaterImpl(defaultWriter,
                                            ignoreSpendableBalanceChanged,
                                            withBlockchainUpdatesEnabled(withNg(wavesSettings)),
                                            ntpTime,
                                            events)

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
          events.onComplete()

          // test goes here
          val updates = events.toListL
            .runSyncUnsafe(5.seconds)

          updates.length shouldBe 6
          updates.head match {
            case BlockAppended(b, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 1
              b.transactionData.length shouldBe 1
              blockStateUpdate.balances.length shouldBe 0
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT
            case _ => fail()
          }

          updates(1) match {
            case MicroBlockAppended(microBlock, height, microBlockStateUpdate, _) =>
              height shouldBe 1
              microBlock.transactionData.length shouldBe 2
              // microBlock transactions miner reward
              microBlockStateUpdate.balances.length shouldBe 1
              microBlockStateUpdate.balances.head._3 shouldBe FEE_AMT * 2 * 0.4
            case _ => fail()
          }

          updates(2) match {
            case MicroBlockAppended(microBlock, height, microBlockStateUpdate, _) =>
              height shouldBe 1
              microBlock.transactionData.length shouldBe 1
              // microBlock transactions miner reward
              microBlockStateUpdate.balances.length shouldBe 1
              microBlockStateUpdate.balances.head._3 shouldBe FEE_AMT * 0.4
            case _ => fail()
          }

          updates(3) match {
            case MicroBlockRollbackCompleted(to, height) =>
              height shouldBe 1
              to shouldBe microBlocks1And2.head.totalResBlockSig
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
