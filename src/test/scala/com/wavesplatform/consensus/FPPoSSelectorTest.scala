package com.wavesplatform.consensus

import java.nio.file.{Files, Path}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.{TransactionGen, WithDB}
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.LevelDBFactory
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state._
import com.wavesplatform.settings._
import com.wavesplatform.state.diffs.{ENOUGH_AMT, ProduceError}
import org.iq80.leveldb.{DB, Options}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.utils.{Time, TimeImpl}
import cats.implicits._
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{BlockchainUpdater, GenesisTransaction}

import scala.concurrent.duration._

/***
  * Tests for PoSSelector with activated FairPoS
  */
class FPPoSSelectorTest extends FreeSpec with Matchers with WithDB with TransactionGen {

  import FPPoSSelectorTest._

  "block delay validation" - {
    "succeed when delay is correct" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miner :: _) =>
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)()

          pos
            .validateBlockDelay(height + 1, block, lastBlock, minerBalance)
            .explicitGet()
      }
    }

    "failed when delay less than expected" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miner :: _) =>
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)(updateDelay = _ - 1)

          pos
            .validateBlockDelay(
              height + 1,
              block,
              lastBlock,
              minerBalance
            ) should produce("less than min valid timestamp")
      }
    }
  }

  "base target validation" - {
    "succed when BT is correct" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miner :: _) =>
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)()

          pos
            .validateBaseTarget(
              height + 1,
              block,
              lastBlock,
              blockchain.blockAt(height - 2)
            ) shouldBe Right(())
      }
    }

    "failed when BT less than expected" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miner :: _) =>
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)(updateBT = _ - 1)

          pos
            .validateBaseTarget(
              height + 1,
              block,
              lastBlock,
              blockchain.blockAt(height - 2)
            ) should produce("does not match calculated baseTarget")
      }
    }

    "failed when BT greater than expected" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miner :: _) =>
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)(updateBT = _ + 1)

          pos
            .validateBaseTarget(
              height + 1,
              block,
              lastBlock,
              blockchain.blockAt(height - 2)
            ) should produce("does not match calculated baseTarget")
      }
    }
  }

  "generation signature validation" - {

  }

  def withEnv(gen: Time => Gen[(Seq[PrivateKeyAccount], Seq[Block])])(f: Env => Unit): Unit = {
    val time          = new TimeImpl
    val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub)
    val settings      = WavesSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, WavesSettings.fromConfig(loadConfig(ConfigFactory.load())), time)
    val pos           = new PoSSelector(bcu, settings.blockchainSettings)
    try {
      val (accounts, blocks) = gen(time).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      f(Env(pos, bcu, accounts))
      bcu.shutdown()
    } finally {
      time.close()
      bcu.shutdown()
      db.close()
    }
  }
}

object FPPoSSelectorTest {
  final case class Env(pos: PoSSelector, blockchain: BlockchainUpdater with NG, miners: Seq[PrivateKeyAccount])

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def forgeBlock(miner: PrivateKeyAccount, blockchain: Blockchain with NG, pos: PoSSelector)(updateDelay: Long => Long = identity,
                                                                                             updateBT: Long => Long = identity,
                                                                                             updateGS: ByteStr => ByteStr = identity): Block = {
    val height       = blockchain.height
    val lastBlock    = blockchain.lastBlock.get
    val ggParentTS   = blockchain.blockAt(height - 2).map(_.timestamp)
    val minerBalance = blockchain.effectiveBalance(miner.toAddress, height, 0)
    val delay = updateDelay(
      pos
        .getValidBlockDelay(
          height,
          miner.publicKey,
          lastBlock.consensusData.baseTarget,
          minerBalance
        )
        .explicitGet()
    )

    val cData = pos
      .consensusData(
        miner.publicKey,
        height,
        60.seconds,
        lastBlock.consensusData.baseTarget,
        lastBlock.timestamp,
        ggParentTS,
        lastBlock.timestamp + delay
      )
      .explicitGet()

    val updatedCData = cData.copy(updateBT(cData.baseTarget), updateGS(cData.generationSignature))

    Block
      .buildAndSign(3: Byte, lastBlock.timestamp + delay, lastBlock.uniqueId, updatedCData, Seq.empty, miner, Set.empty)
      .explicitGet()
  }

  val accountGen: Gen[PrivateKeyAccount] =
    Gen
      .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
      .map(PrivateKeyAccount.apply)

  def chainGen(balances: List[Long], blockCount: Int)(t: Time): Gen[(Seq[PrivateKeyAccount], Seq[Block])] = {
    val ts = t.correctedTime()

    Gen
      .listOfN(balances.length, accountGen)
      .map(_ zip balances)
      .map { accountsWithBalances =>
        for {
          (acc, balance) <- accountsWithBalances
          i = accountsWithBalances.indexOf((acc, balance))
        } yield (acc, GenesisTransaction.create(acc, balance, ts + i).explicitGet())
      }
      .map { txs =>
        val lastTxTimestamp = txs.lastOption.map(_._2.timestamp) getOrElse ts
        val genesisBlock    = TestBlock.create(lastTxTimestamp + 1, txs.map(_._2))

        val chain = (1 to blockCount foldLeft List(genesisBlock)) { (blocks, d) =>
          val newBlock = TestBlock
            .create(
              lastTxTimestamp + 1 + d,
              blocks.head.uniqueId,
              Seq.empty
            )
          newBlock :: blocks
        }

        (txs.map(_._1), chain.reverse)
      }
  }

}
