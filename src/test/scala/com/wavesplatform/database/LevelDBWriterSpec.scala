package com.wavesplatform.database

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.block.Block
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BlockchainUpdaterImpl, EitherExt2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionV1}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{RequestGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}

class LevelDBWriterSpec extends FreeSpec with Matchers with WithDB with RequestGen {
  "Slice" - {
    "drops tail" in {
      LevelDBWriter.slice(Seq(10, 7, 4), 7, 10) shouldEqual Seq(10, 7)
    }
    "drops head" in {
      LevelDBWriter.slice(Seq(10, 7, 4), 4, 8) shouldEqual Seq(7, 4)
    }
    "includes Genesis" in {
      LevelDBWriter.slice(Seq(10, 7), 5, 11) shouldEqual Seq(10, 7, 1)
    }
  }
  "Merge" - {
    import TestFunctionalitySettings.Enabled
    "correctly joins height ranges" in {
      val fs     = Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccountTrading.id -> 0))
      val writer = new LevelDBWriter(db, fs, 100000, 2000, 120 * 60 * 1000)
      writer.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 5))
      writer.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 3))
      writer.merge(Seq(8, 4), Seq(8, 4)) shouldEqual Seq((8, 8), (4, 4))
    }

    "preserves compatibility until SmartAccountTrading feature is activated" in {
      val writer = new LevelDBWriter(db, Enabled, 100000, 2000, 120 * 60 * 1000)
      writer.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 12), (3, 5))
      writer.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 12), (5, 3))
      writer.merge(Seq(8, 4), Seq(8, 4)) shouldEqual Seq((8, 8), (4, 8), (4, 4))
    }
  }
  "hasScript" - {
    "returns false if a script was not set" in {
      val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub, 100000, 2000, 120 * 60 * 1000)
      writer.hasScript(accountGen.sample.get.toAddress) shouldBe false
    }

    "returns false if a script was set and then unset" in {
      assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
      resetTest { (_, account) =>
        val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub, 100000, 2000, 120 * 60 * 1000)
        writer.hasScript(account) shouldBe false
      }
    }

    "returns true" - {
      "if there is a script in db" in {
        assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
        test { (_, account) =>
          val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub, 100000, 2000, 120 * 60 * 1000)
          writer.hasScript(account) shouldBe true
        }
      }

      "if there is a script in cache" in {
        assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
        test { (defaultWriter, account) =>
          defaultWriter.hasScript(account) shouldBe true
        }
      }
    }

    def gen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val nextBlock = TestBlock.create(ts + 1, blocks.last.uniqueId, Seq())
        (master, blocks :+ nextBlock)
    }

    def resetGen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val unsetScriptTx = SetScriptTransaction
          .selfSigned(1, master, None, 5000000, ts + 1)
          .explicitGet()

        val block1 = TestBlock.create(ts + 1, blocks.last.uniqueId, Seq(unsetScriptTx))
        val block2 = TestBlock.create(ts + 2, block1.uniqueId, Seq())
        (master, blocks ++ List(block1, block2))
    }

    def baseGen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = accountGen.map { master =>
      val genesisTx = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      val setScriptTx = SetScriptTransaction
        .selfSigned(1, master, Some(ScriptV1(Terms.TRUE).explicitGet()), 5000000, ts)
        .explicitGet()

      val block = TestBlock.create(ts, Seq(genesisTx, setScriptTx))
      (master, Seq(block))
    }

    def test(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = baseTest(t => gen(t.correctedTime()))(f)

    def resetTest(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = baseTest(t => resetGen(t.correctedTime()))(f)

  }

  def baseTest(gen: Time => Gen[(PrivateKeyAccount, Seq[Block])])(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = {
    val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub, 100000, 2000, 120 * 60 * 1000)
    val settings0     = WavesSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, settings, ntpTime)
    try {
      val (account, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      bcu.shutdown()
      f(defaultWriter, account)
    } finally {
      bcu.shutdown()
      db.close()
    }
  }

  "addressTransactions" - {
    def preconditions(ts: Long): Gen[(PrivateKeyAccount, List[Block])] = {
      for {
        master    <- accountGen
        recipient <- accountGen
        genesisBlock = TestBlock
          .create(ts, Seq(GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()))
        block1 = TestBlock
          .create(
            ts + 3,
            genesisBlock.uniqueId,
            Seq(
              createTransfer(master, recipient.toAddress, ts + 1),
              createTransfer(master, recipient.toAddress, ts + 2)
            )
          )
        emptyBlock = TestBlock.create(ts + 5, block1.uniqueId, Seq())
      } yield (master, List(genesisBlock, block1, emptyBlock))
    }

    "return txs in correct ordering without fromId" in {
      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        val txs = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 3, None)
          .explicitGet()

        val ordering = Ordering
          .by[(Int, Transaction), (Int, Long)]({ case (h, t) => (-h, -t.timestamp) })

        txs.length shouldBe 2

        txs.sorted(ordering) shouldBe txs
      }
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
      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        val nonExistentTxId = GenesisTransaction.create(account, ENOUGH_AMT, 1).explicitGet().id()

        val txs = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 3, Some(nonExistentTxId))

        txs shouldBe Left(s"Transaction $nonExistentTxId does not exist")
      }
    }

    "return txs in correct ordering starting from a given id" in {
      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        // using pagination
        val firstTx = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 1, None)
          .explicitGet()
          .head

        val secondTx = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 1, Some(firstTx._2.id()))
          .explicitGet()
          .head

        // without pagination
        val txs = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 2, None)
          .explicitGet()

        txs shouldBe Seq(firstTx, secondTx)
      }
    }

    "return an empty Seq when paginating from the last transaction" in {
      baseTest(time => preconditions(time.correctedTime())) { (writer, account) =>
        val txs = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 2, None)
          .explicitGet()

        val txsFromLast = writer
          .addressTransactions(account.toAddress, Set(TransferTransactionV1.typeId), 2, Some(txs.last._2.id()))
          .explicitGet()

        txs.length shouldBe 2
        txsFromLast shouldBe Seq.empty
      }
    }

    def createTransfer(master: PrivateKeyAccount, recipient: Address, ts: Long): TransferTransaction = {
      TransferTransactionV1
        .selfSigned(None, master, recipient, ENOUGH_AMT / 5, ts, None, 1000000, Array.emptyByteArray)
        .explicitGet()
    }
  }
}
