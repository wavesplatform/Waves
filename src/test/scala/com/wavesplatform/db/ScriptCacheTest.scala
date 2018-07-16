package com.wavesplatform.db

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.{BlockchainUpdaterImpl, _}
import com.wavesplatform.{TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.GenesisTransaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.{Script, ScriptCompiler}
import scorex.utils.{Time, TimeImpl}

class ScriptCacheTest extends FreeSpec with Matchers with WithDB with TransactionGen {

  val CACHE_SIZE = 1
  val AMOUNT     = 10000000000l
  val FEE        = 5000000

  def blockGen(scripts: List[Script], t: Time): Gen[(Seq[PrivateKeyAccount], Seq[Block])] = {
    val ts = t.correctedTime()
    Gen
      .listOfN(scripts.length, accountGen)
      .map { accounts =>
        for {
          account <- accounts
          i = accounts.indexOf(account)
        } yield (account, GenesisTransaction.create(account.toAddress, AMOUNT, ts + i).explicitGet())
      }
      .map { ag =>
        val (accounts, genesisTxs) = ag.unzip

        val setScriptTxs =
          (accounts zip scripts)
            .map {
              case (account, script) =>
                SetScriptTransaction
                  .selfSigned(1, account, Some(script), FEE, ts + accounts.length + accounts.indexOf(account) + 1)
                  .explicitGet()
            }

        val genesisBlock = TestBlock.create(genesisTxs)

        val nextBlock =
          TestBlock
            .create(
              time = setScriptTxs.last.timestamp + 1,
              ref = genesisBlock.uniqueId,
              txs = setScriptTxs
            )

        (accounts, genesisBlock +: nextBlock +: Nil)
      }
  }

  "ScriptCache" - {
    "return correct script after overflow" in {
      val scripts =
        (0 to CACHE_SIZE * 10).map { ind =>
          val (script, _) = ScriptCompiler(
            s"""
            |let ind = $ind
            |true
          """.stripMargin
          ).explicitGet()

          script
        }.toList

      withLevelDB(blockGen(scripts, _)) {
        case (accounts, db) =>
          val allScriptCorrect = (accounts zip scripts)
            .map {
              case (account, script) =>
                val address = account.toAddress

                val scriptFromCache =
                  db.accountScript(address)
                    .toRight(s"No script for acc: $account")
                    .explicitGet()

                scriptFromCache == script && db.hasScript(address)
            }
            .forall(identity)

          allScriptCorrect shouldBe true
      }
    }
  }

  def withLevelDB(gen: Time => Gen[(Seq[PrivateKeyAccount], Seq[Block])])(f: (Seq[PrivateKeyAccount], Blockchain) => Unit): Unit = {
    val time          = new TimeImpl
    val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub, CACHE_SIZE)
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, WavesSettings.fromConfig(loadConfig(ConfigFactory.load())), time)
    try {
      val (accounts, blocks) = gen(time).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      f(accounts, bcu)
      bcu.shutdown()
    } finally {
      time.close()
      bcu.shutdown()
      db.close()
    }
  }
}
