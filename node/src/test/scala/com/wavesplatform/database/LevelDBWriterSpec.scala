package com.wavesplatform.database

import java.nio.BufferUnderflowException

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.TransactionMeta
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.{GenesisSettings, TestFunctionalitySettings, TestSettings, WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils._
import com.wavesplatform.state.{BlockchainUpdaterImpl, Height, TransactionId, TxMeta, TxNum}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.utils.{SystemTime, Time}
import com.wavesplatform.{RequestGen, WithDB, database}
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration.Duration

//noinspection NameBooleanParameters
class LevelDBWriterSpec
    extends FreeSpec
    with WithDB
    with DBCacheSettings
    with RequestGen {
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
    "correctly joins height ranges" in {
      LevelDBWriter.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 5))
      LevelDBWriter.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 3))
      LevelDBWriter.merge(Seq(8, 4), Seq(8, 4)) shouldEqual Seq((8, 8), (4, 4))
    }
  }
  "hasScript" - {
    "returns false if a script was not set" in {
      val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)
      writer.hasAccountScript(accountGen.sample.get.toAddress) shouldBe false
    }

    "returns false if a script was set and then unset" in {
      assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
      resetTest { (_, account) =>
        val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)
        writer.hasAccountScript(account.toAddress) shouldBe false
      }
    }

    "returns true" - {
      "if there is a script in db" in {
        assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
        test { (_, account) =>
          val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)
          writer.hasAccountScript(account.toAddress) shouldBe true
        }
      }

      "if there is a script in cache" in {
        assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
        test { (defaultWriter, account) =>
          defaultWriter.hasAccountScript(account.toAddress) shouldBe true
        }
      }
    }

    def gen(ts: Long): Gen[(KeyPair, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val nextBlock = TestBlock.create(ts + 1, blocks.last.id(), Seq())
        (master, blocks :+ nextBlock)
    }

    def resetGen(ts: Long): Gen[(KeyPair, Seq[Block])] = baseGen(ts).map {
      case (master, blocks) =>
        val unsetScriptTx = SetScriptTransaction
          .selfSigned(1.toByte, master, None, 5000000, ts + 1)
          .explicitGet()

        val block1 = TestBlock.create(ts + 1, blocks.last.id(), Seq(unsetScriptTx))
        val block2 = TestBlock.create(ts + 2, block1.id(), Seq())
        (master, blocks ++ List(block1, block2))
    }

    def baseGen(ts: Long): Gen[(KeyPair, Seq[Block])] = accountGen.map { master =>
      val genesisTx = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      val setScriptTx = SetScriptTransaction
        .selfSigned(1.toByte, master, Some(ExprScript(Terms.TRUE).explicitGet()), 5000000, ts)
        .explicitGet()

      val block = TestBlock.create(ts, Seq(genesisTx, setScriptTx))
      (master, Seq(block))
    }

    def test(f: (LevelDBWriter, KeyPair) => Unit): Unit = baseTest(t => gen(t.correctedTime()))(f)

    def resetTest(f: (LevelDBWriter, KeyPair) => Unit): Unit = baseTest(t => resetGen(t.correctedTime()))(f)

  }

  "wavesAmount" - {
    "counts genesis" in {
      val (_, leveldb) = TestStorageFactory(
        TestSettings.Default.copy(
          blockchainSettings =
            TestSettings.Default.blockchainSettings.copy(genesisSettings = GenesisSettings(0L, 0L, 1234L, None, Nil, 1, Duration.Zero))
        ),
        this.db,
        SystemTime,
        ignoreSpendableBalanceChanged,
        BlockchainUpdateTriggers.noop
      )

      leveldb.wavesAmount(1) shouldBe BigInt(1234)
    }
  }

  def baseTest(gen: Time => Gen[(KeyPair, Seq[Block])])(f: (LevelDBWriter, KeyPair) => Unit): Unit = {
    val defaultWriter = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)
    val settings0     = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu =
      new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    try {
      val (account, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block, block.header.generationSignature) should beRight
      }

      bcu.shutdown()
      f(defaultWriter, account)
    } finally {
      bcu.shutdown()
      db.close()
    }
  }

  def testWithBlocks(gen: Time => Gen[(KeyPair, Seq[Block])])(f: (LevelDBWriter, Seq[Block], KeyPair) => Unit): Unit = {
    val defaultWriter = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)
    val settings0     = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu =
      new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    try {
      val (account, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block, block.header.generationSignature) should beRight
      }

      bcu.shutdown()
      f(defaultWriter, blocks, account)
    } finally {
      bcu.shutdown()
      db.close()
    }
  }

  def createTransfer(master: KeyPair, recipient: Address, ts: Long): TransferTransaction =
    TransferTransaction
      .selfSigned(1.toByte, master, recipient, Waves, ENOUGH_AMT / 10, Waves, 1000000, ByteStr.empty, ts)
      .explicitGet()

  def preconditions(ts: Long): Gen[(KeyPair, List[Block])] = {
    for {
      master    <- accountGen
      recipient <- accountGen
      genesisBlock = TestBlock
        .create(ts, Seq(GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()))
      block1 = TestBlock
        .create(
          ts + 3,
          genesisBlock.id(),
          Seq(
            createTransfer(master, recipient.toAddress, ts + 1),
            createTransfer(master, recipient.toAddress, ts + 2)
          )
        )
      emptyBlock = TestBlock.create(ts + 5, block1.id(), Seq())
    } yield (master, List(genesisBlock, block1, emptyBlock))
  }

  "correctly reassemble block from header and transactions" in {
    val rw = TestLevelDB.withFunctionalitySettings(
      db,
      ignoreSpendableBalanceChanged,
      TestFunctionalitySettings.Stub.copy(preActivatedFeatures = Map(15.toShort -> 5))
    )
    val settings0 = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings  = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu       = new BlockchainUpdaterImpl(rw, ignoreSpendableBalanceChanged, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    try {
      val master    = KeyPair(ByteStr("master".getBytes()))
      val recipient = KeyPair(ByteStr("recipient".getBytes()))

      val ts = System.currentTimeMillis()

      val genesisBlock = TestBlock
        .create(ts, Seq(GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()))
      val block1 = TestBlock
        .create(
          ts + 3,
          genesisBlock.id(),
          Seq(
            createTransfer(master, recipient.toAddress, ts + 1),
            createTransfer(master, recipient.toAddress, ts + 2)
          )
        )
      val block2 = TestBlock.create(ts + 5, block1.id(), Seq())
      val block3 = TestBlock
        .create(
          ts + 10,
          block2.id(),
          Seq(
            createTransfer(master, recipient.toAddress, ts + 6),
            createTransfer(master, recipient.toAddress, ts + 7)
          )
        )

      val block4 = TestBlock
        .create(
          ts + 17,
          block3.id(),
          Seq(
            createTransfer(master, recipient.toAddress, ts + 13),
            createTransfer(master, recipient.toAddress, ts + 14)
          ),
          version = Block.ProtoBlockVersion
        )

      val block5 = TestBlock
        .create(
          ts + 24,
          block4.id(),
          Seq(
            createTransfer(master, recipient.toAddress, ts + 20),
            createTransfer(master, recipient.toAddress, ts + 21)
          ),
          version = Block.ProtoBlockVersion
        )

      bcu.processBlock(genesisBlock, genesisBlock.header.generationSignature) should beRight
      bcu.processBlock(block1, block1.header.generationSignature) should beRight
      bcu.processBlock(block2, block2.header.generationSignature) should beRight
      bcu.processBlock(block3, block3.header.generationSignature) should beRight
      bcu.processBlock(block4, block4.header.generationSignature) should beRight
      bcu.processBlock(block5, block5.header.generationSignature) should beRight

      def blockAt(height: Int): Option[Block] =
        bcu.liquidBlockMeta
          .filter(_ => bcu.height == height)
          .flatMap(m => bcu.liquidBlock(m.id))
          .orElse(db.readOnly(ro => database.loadBlock(Height(height), ro)))

      blockAt(1).get shouldBe genesisBlock
      blockAt(2).get shouldBe block1
      blockAt(3).get shouldBe block2
      blockAt(4).get shouldBe block3
      blockAt(5).get shouldBe block4
      blockAt(6).get shouldBe block5

      for (i <- 1 to db.get(Keys.height)) {
        db.get(Keys.blockMetaAt(Height(i))).isDefined shouldBe true
      }

    } finally {
      bcu.shutdown()
      db.close()
    }
  }

  "addressTransactions" - {

    "don't parse irrelevant transactions in transferById" ignore {
      val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)

      forAll(randomTransactionGen) { tx =>
        val transactionId = tx.id()
        db.put(Keys.transactionMetaById(TransactionId @@ transactionId).keyBytes, TransactionMeta(1, 0, tx.tpe.id, true).toByteArray)
        db.put(Keys.transactionAt(Height @@ 1, TxNum @@ 0.toShort).keyBytes, Array[Byte](1, 2, 3, 4, 5, 6))

        writer.transferById(transactionId) shouldBe None

        db.put(Keys.transactionAt(Height @@ 1, TxNum @@ 0.toShort).keyBytes, Array[Byte](TransferTransaction.typeId, 2, 3, 4, 5, 6))
        intercept[BufferUnderflowException](writer.transferById(transactionId))
      }
    }
  }

  "readTransactionBytes" - {
    "reads correct failed transactions" in {
      val writer = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub)

      val scenario =
        for {
          oldBytesTx <- randomTransactionGen.map(tx => (tx, true))
          newBytesTx <- Gen
            .oneOf(
              exchangeTransactionGen.map(tx => tx.copy(version = TxVersion.V3)),
              invokeScriptGen(paymentListGen).map(tx => tx.copy(version = TxVersion.V3))
            )
            .flatMap(tx => Arbitrary.arbBool.arbitrary.map(s => (tx, s)))
          (tx, status) <- Gen.oneOf(oldBytesTx, newBytesTx)
        } yield (tx, status)

      forAll(scenario) {
        case (tx, s) =>
          val transactionId = tx.id()
          db.put(Keys.transactionMetaById(TransactionId(transactionId)).keyBytes, TransactionMeta(1, 0, tx.tpe.id, !s).toByteArray)
          db.put(Keys.transactionAt(Height(1), TxNum(0.toShort)).keyBytes, database.writeTransaction((TxMeta(Height(1), s, 0L), tx)))

          writer.transactionInfo(transactionId) shouldBe Some(TxMeta(Height(1), s, 0L) -> tx)
      }
    }
  }
}
