package com.wavesplatform.database

import com.typesafe.config.ConfigFactory
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BlockchainUpdaterImpl, EitherExt2}
import com.wavesplatform.{RequestGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.utils.TimeImpl

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
    "correctly joins height ranges" in {
      LevelDBWriter.merge(Seq(15, 12, 3), Seq(12, 5)) shouldEqual Seq((15, 12), (12, 12), (3, 12), (3, 5))
      LevelDBWriter.merge(Seq(12, 5), Seq(15, 12, 3)) shouldEqual Seq((12, 15), (12, 12), (5, 12), (5, 3))
    }
  }
  "hasScript" - {
    "returns false if a script was not set" in {
      val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
      writer.hasScript(accountGen.sample.get.toAddress) shouldBe false
    }

    "returns true" - {
      "if there is a script in db" in test { (_, account) =>
        val writer = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
        writer.hasScript(account) shouldBe true
      }

      "if there is a script in cache" in test { (defaultWriter, account) =>
        defaultWriter.hasScript(account) shouldBe true
      }
    }

    def gen(ts: Long): Gen[(PrivateKeyAccount, Seq[Block])] = accountGen.map { master =>
      val genesisTx = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      val setScriptTx = SetScriptTransaction
        .selfSigned(1, master, Some(ScriptV1(Terms.TRUE).explicitGet()), ENOUGH_AMT, ts)
        .explicitGet()

      val block1 = TestBlock.create(ts, Seq(genesisTx, setScriptTx))
      val block2 = TestBlock.create(ts + 1, block1.uniqueId, Seq())
      (master, Seq(block1, block2))
    }

    def test(f: (LevelDBWriter, PrivateKeyAccount) => Unit): Unit = {
      val time          = new TimeImpl
      val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
      val bcu           = new BlockchainUpdaterImpl(defaultWriter, WavesSettings.fromConfig(loadConfig(ConfigFactory.load())), time)
      try {
        val (account, blocks) = gen(time.correctedTime()).sample.get
        blocks.foreach { block =>
          bcu.processBlock(block).explicitGet()
        }

        bcu.shutdown()
        f(defaultWriter, account)
      } finally {
        time.close()
        bcu.shutdown()
        db.close()
      }
    }
  }
}
