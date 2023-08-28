package com.wavesplatform.db

import com.typesafe.config.ConfigFactory
import com.wavesplatform.WithNewDBForEachTest
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.settings.{TestFunctionalitySettings, WavesSettings, loadConfig}
import com.wavesplatform.state.*
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.Time
import org.scalacheck.Gen

class ScriptCacheTest extends FreeSpec with WithNewDBForEachTest {

  val CACHE_SIZE = 1
  val AMOUNT     = 10000000000L
  val FEE        = 5000000

  def mkScripts(num: Int): List[(Script, Long)] = {
    (0 until num).map { ind =>
      ScriptCompiler(
        s"""
           |let ind = $ind
           |true
          """.stripMargin,
        isAssetScript = false,
        ScriptEstimatorV2
      ).explicitGet()
    }.toList
  }

  def blockGen(scripts: List[(Script, Long)], t: Time): Gen[(Seq[KeyPair], Seq[Block])] = {
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
            .map { case (account, (script, _)) =>
              SetScriptTransaction
                .selfSigned(1.toByte, account, Some(script), FEE, ts + accounts.length + accounts.indexOf(account) + 1)
                .explicitGet()
            }

        val genesisBlock = TestBlock.create(genesisTxs)

        val nextBlock =
          TestBlock
            .create(
              time = setScriptTxs.last.timestamp + 1,
              ref = genesisBlock.id(),
              txs = setScriptTxs
            )

        (accounts, genesisBlock +: nextBlock +: Nil)
      }
  }

  "ScriptCache" - {
    "return correct script after overflow" in {
      val scripts = mkScripts(CACHE_SIZE * 10)

      withBlockchain(blockGen(scripts, _)) { case (accounts, bc) =>
        val allScriptCorrect = (accounts zip scripts)
          .map { case (account, (script, _)) =>
            val address = account.toAddress

            val scriptFromCache =
              bc.accountScript(address)
                .map(_.script)
                .toRight(s"No script for acc: $account")
                .explicitGet()

            scriptFromCache == script && bc.hasAccountScript(address)
          }
          .forall(identity)

        allScriptCorrect shouldBe true
      }
    }

    "Return correct script after rollback" in {
      val scripts              = mkScripts(1)
      val (script, complexity) = scripts.head

      withBlockchain(blockGen(scripts, _)) { case (accounts, bcu) =>
        val account = accounts.head
        bcu.accountScript(account.toAddress) shouldEqual Some(AccountScriptInfo(account.publicKey, script, complexity))

        val lastBlockHeader = bcu.lastBlockHeader.get

        val newScriptTx = SetScriptTransaction
          .selfSigned(1.toByte, account, None, FEE, lastBlockHeader.header.timestamp + 1)
          .explicitGet()

        val blockWithEmptyScriptTx = TestBlock
          .create(
            time = lastBlockHeader.header.timestamp + 2,
            ref = lastBlockHeader.id(),
            txs = Seq(newScriptTx)
          )

        bcu
          .processBlock(blockWithEmptyScriptTx, blockWithEmptyScriptTx.header.generationSignature, None)
          .explicitGet()

        bcu.accountScript(account.toAddress) shouldEqual None
        bcu.removeAfter(lastBlockHeader.id())
        bcu.accountScript(account.toAddress).map(_.script) shouldEqual Some(script)
      }
    }

  }

  def withBlockchain(gen: Time => Gen[(Seq[KeyPair], Seq[Block])])(f: (Seq[KeyPair], Blockchain & BlockchainUpdater) => Unit): Unit = {
    val settings0 = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings  = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val defaultWriter = TestRocksDB.withFunctionalitySettings(
      db,
      TestFunctionalitySettings.Stub
    )
    val bcu =
      new BlockchainUpdaterImpl(defaultWriter, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    try {
      val (accounts, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block, block.header.generationSignature, None) should beRight
      }

      f(accounts, bcu)
      bcu.shutdown()
    } finally {
      bcu.shutdown()
    }
  }
}
