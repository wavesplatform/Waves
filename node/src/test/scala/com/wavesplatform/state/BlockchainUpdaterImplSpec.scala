package com.wavesplatform.state

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionV1}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{NTPTime, RequestGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}

class BlockchainUpdaterImplSpec extends FreeSpec with Matchers with WithDB with RequestGen with NTPTime with DBCacheSettings {

  def baseTest(gen: Time => Gen[(KeyPair, Seq[Block])])(f: (BlockchainUpdaterImpl, KeyPair) => Unit): Unit = {
    val defaultWriter = new LevelDBWriter(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub, dbSettings)
    val settings      = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
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
    TransferTransactionV1
      .selfSigned(Waves, master, recipient, ENOUGH_AMT / 5, ts, Waves, 1000000, Array.emptyByteArray)
      .explicitGet()
  }

  "addressTransactions" - {
    def preconditions(ts: Long): Gen[(KeyPair, List[Block])] = {
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

    "correctly applies transaction type filter" in {
      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        val txs = writer
          .addressTransactions(account.toAddress, Set(GenesisTransaction.typeId), 10, None)
          .explicitGet()

        txs.length shouldBe 1
      }
    }

    "return Left if fromId argument is a non-existent transaction" in {
      baseTest(time => preconditions(time.correctedTime())) { (updater, account) =>
        val nonExistentTxId = GenesisTransaction.create(account, ENOUGH_AMT, 1).explicitGet().id()

        val txs = updater
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 3, Some(nonExistentTxId))

        txs shouldBe Left(s"Transaction $nonExistentTxId does not exist")
      }
    }

    "without pagination" in {
      baseTest(time => preconditions(time.correctedTime())) { (updater, account) =>
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
        baseTest(time => preconditions(time.correctedTime())) { (updater, account) =>
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

}
