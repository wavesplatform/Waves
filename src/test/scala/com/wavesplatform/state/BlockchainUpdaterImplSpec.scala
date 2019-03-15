package com.wavesplatform.state

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
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

  private def withNg(settings: FunctionalitySettings): FunctionalitySettings =
    settings.copy(
      blockVersion3AfterHeight = 0,
      preActivatedFeatures = settings.preActivatedFeatures ++ Map(BlockchainFeatures.NG.id -> 0)
    )

  private def withNg(settings: WavesSettings): WavesSettings =
    settings.copy(
      blockchainSettings = settings.blockchainSettings.copy(functionalitySettings = withNg(settings.blockchainSettings.functionalitySettings)))

  def baseTest(gen: Time => Gen[(PrivateKeyAccount, Seq[Block])], enableNg: Boolean = false, events: Option[Observer[BlockchainUpdated]] = None)(
      f: (BlockchainUpdaterImpl, PrivateKeyAccount) => Unit): Unit = {
    val (fs, settings) = {
      val defaultFs       = TestFunctionalitySettings.Stub
      val defaultSettings = WavesSettings.fromConfig(loadConfig(ConfigFactory.load()))
      if (enableNg) (withNg(defaultFs), withNg(defaultSettings)) else (defaultFs, defaultSettings)
    }
    val defaultWriter = new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, maxCacheSize, 2000, 120 * 60 * 1000)
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime, events)
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

  def createTransfer(master: PrivateKeyAccount, recipient: Address, ts: Long): TransferTransaction = {
    TransferTransactionV1
      .selfSigned(None, master, recipient, ENOUGH_AMT / 5, ts, None, FEE_AMT, Array.emptyByteArray)
      .explicitGet()
  }

  def commonPreconditions(ts: Long): Gen[(PrivateKeyAccount, List[Block])] = {
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
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = false, Some(events)) { (_, _) =>
          val updates = events.toListL
            .runSyncUnsafe(5.seconds)

          updates.length shouldBe 3

          // genesis block
          updates.head match {
            case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 1
              block.transactionData.length shouldBe 1
              blockStateUpdate.balances.length shouldBe 0
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT
          }

          // transfers block
          updates(1) match {
            case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 2
              block.transactionData.length shouldBe 5
              // miner reward, no NG — all txs fees
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe FEE_AMT * 5

              // first Tx updated balances
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT / 5
              transactionsStateUpdates.head.balances.last._3 shouldBe (ENOUGH_AMT - ENOUGH_AMT / 5 - FEE_AMT)
          }
        }
      }
    }

    "with NG" - {
      "genesis block and two transfers blocks" in {
        val events = ReplaySubject[BlockchainUpdated]()
        baseTest(time => commonPreconditions(time.correctedTime()), enableNg = true, Some(events)) { (_, _) =>
          val updates = events.toListL
            .runSyncUnsafe(5.seconds)

          updates.length shouldBe 3

          // genesis block, same as without NG
          updates.head match {
            case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 1
              block.transactionData.length shouldBe 1
              blockStateUpdate.balances.length shouldBe 0
              transactionsStateUpdates.head.balances.head._3 shouldBe ENOUGH_AMT
          }

          // first transfers block
          updates(1) match {
            case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 2
              block.transactionData.length shouldBe 5

              // miner reward, with NG — 40% of all txs fees
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe FEE_AMT * 5 * 0.4
          }

          // second transfers block, with carryFee
          updates(2) match {
            case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
              height shouldBe 3
              block.transactionData.length shouldBe 4

              // miner reward, with NG — 40% of all txs fees and 60% from previous block
              blockStateUpdate.balances.length shouldBe 1
              blockStateUpdate.balances.head._3 shouldBe (
                FEE_AMT * 5 * 0.4     // miner balance from prev block
                  + FEE_AMT * 4 * 0.4 // carry from prev block
                  + FEE_AMT * 5 * 0.6 // current block reward
              )
          }
        }
      }

      // @todo write process microblock and rollback tests
    }
  }
}
