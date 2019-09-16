package com.wavesplatform.consensus

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{WavesSettings, _}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{TransactionGen, WithDB}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class FPPoSSelectorTest extends FreeSpec with Matchers with WithDB with TransactionGen with DBCacheSettings {

  import FPPoSSelectorTest._

  "block delay" - {
    "same on the same height in different forks" in {
      withEnv(chainGen(List(ENOUGH_AMT / 2, ENOUGH_AMT / 3), 110)) {
        case Env(_, blockchain, miners) =>
          val miner1 = miners.head
          val miner2 = miners.tail.head

          val miner1Balance = blockchain.effectiveBalance(miner1.toAddress, 0)

          val fork1 = mkFork(10, miner1, blockchain)
          val fork2 = mkFork(10, miner2, blockchain)

          val fork1Delay = {
            val blockForHit =
              fork1
                .lift(100)
                .orElse(blockchain.blockAt(blockchain.height + fork1.length - 100))
                .getOrElse(fork1.head)

            calcDelay(blockForHit, fork1.head.consensusData.baseTarget, miner1, miner1Balance)
          }

          val fork2Delay = {
            val blockForHit =
              fork2
                .lift(100)
                .orElse(blockchain.blockAt(blockchain.height + fork2.length - 100))
                .getOrElse(fork2.head)

            calcDelay(blockForHit, fork2.head.consensusData.baseTarget, miner1, miner1Balance)
          }

          fork1Delay shouldEqual fork2Delay
      }
    }
  }

  "block delay validation" - {
    "succeed when delay is correct" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miners) =>
          val miner        = miners.head
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
          val lastBlock    = blockchain.lastBlock.get
          val block        = forgeBlock(miner, blockchain, pos)()

          pos
            .validateBlockDelay(height + 1, block, lastBlock, minerBalance)
            .explicitGet()
      }
    }

    "failed when delay less than expected" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miners) =>
          val miner        = miners.head
          val height       = blockchain.height
          val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
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
    "succeed when BT is correct" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miners) =>
          val miner     = miners.head
          val height    = blockchain.height
          val lastBlock = blockchain.lastBlock.get
          val block     = forgeBlock(miner, blockchain, pos)()

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
        case Env(pos, blockchain, miners) =>
          val miner     = miners.head
          val height    = blockchain.height
          val lastBlock = blockchain.lastBlock.get
          val block     = forgeBlock(miner, blockchain, pos)(updateBT = _ - 1)

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
        case Env(pos, blockchain, miners) =>
          val miner     = miners.head
          val height    = blockchain.height
          val lastBlock = blockchain.lastBlock.get
          val block     = forgeBlock(miner, blockchain, pos)(updateBT = _ + 1)

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
    "succeed when GS is correct" in {
      withEnv(chainGen(List(ENOUGH_AMT), 10)) {
        case Env(pos, blockchain, miners) =>
          val miner  = miners.head
          val height = blockchain.height
          val block  = forgeBlock(miner, blockchain, pos)()

          pos
            .validateGeneratorSignature(
              height + 1,
              block
            ) shouldBe Right(())
      }
    }

    "failed when GS is incorrect" in {
      withEnv(chainGen(List(ENOUGH_AMT), 100)) {
        case Env(pos, blockchain, miners) =>
          val miner  = miners.head
          val height = blockchain.height
          val block  = forgeBlock(miner, blockchain, pos)(updateGS = gs => ByteStr(gs.arr |< Random.nextBytes))

          pos
            .validateGeneratorSignature(
              height + 1,
              block
            ) should produce("Generation signatures does not match")
      }
    }
  }

  "regression" - {
    "delay" in {
      FairPoSCalculator.calculateDelay(BigInt(1), 100l, 10000000000000l) shouldBe 705491
      FairPoSCalculator.calculateDelay(BigInt(2), 200l, 20000000000000l) shouldBe 607358
      FairPoSCalculator.calculateDelay(BigInt(3), 300l, 30000000000000l) shouldBe 549956
    }

    "base target" in {
      FairPoSCalculator.calculateBaseTarget(100l, 30, 100l, 100000000000l, Some(99000l), 100000l) shouldBe 99l
      FairPoSCalculator.calculateBaseTarget(100l, 10, 100l, 100000000000l, None, 100000000000l) shouldBe 100l
      FairPoSCalculator.calculateBaseTarget(100l, 10, 100l, 100000000000l, Some(99999700000l), 100000000000l) shouldBe 100l
      FairPoSCalculator.calculateBaseTarget(100l, 30, 100l, 100000000000l, Some(1l), 1000000l) shouldBe 101l
    }
  }

  def withEnv(gen: Time => Gen[(Seq[KeyPair], Seq[Block])])(f: Env => Unit): Unit = {
    val defaultWriter = TestLevelDB.withFunctionalitySettings(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub, dbSettings)
    val settings0     = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings      = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu           = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
    val pos           = new PoSSelector(bcu, settings.blockchainSettings, settings.synchronizationSettings)
    try {
      val (accounts, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block).explicitGet()
      }

      f(Env(pos, bcu, accounts))
      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
    }
  }
}

object FPPoSSelectorTest {

  implicit class KComb[A](a: A) {
    def |<(f: A => Unit): A = {
      f(a)
      a
    }
  }

  final case class Env(pos: PoSSelector, blockchain: BlockchainUpdater with NG, miners: Seq[KeyPair])

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def mkFork(blockCount: Int, miner: KeyPair, blockchain: Blockchain): List[Block] = {
    val height = blockchain.height

    val lastBlock = blockchain.lastBlock.get

    ((1 to blockCount) foldLeft List(lastBlock)) { (forkChain, ind) =>
      val blockForHit =
        forkChain
          .lift(100)
          .orElse(blockchain.blockAt(height + ind - 100))
          .getOrElse(forkChain.head)

      val gs =
        PoSCalculator
          .generatorSignature(
            blockForHit.consensusData.generationSignature.arr,
            miner
          )

      val delay: Long = 60000

      val bt = FairPoSCalculator.calculateBaseTarget(
        60,
        height + ind - 1,
        forkChain.head.consensusData.baseTarget,
        forkChain.head.timestamp,
        (forkChain.lift(2) orElse blockchain.blockAt(height + ind - 3)) map (_.timestamp),
        forkChain.head.timestamp + delay
      )

      val newBlock = Block
        .buildAndSign(
          3: Byte,
          forkChain.head.timestamp + delay,
          forkChain.head.uniqueId,
          NxtLikeConsensusBlockData(bt, ByteStr(gs)),
          Seq.empty,
          miner,
          Set.empty,
          -1L
        )
        .explicitGet()

      newBlock :: forkChain
    }
  }

  def forgeBlock(miner: KeyPair, blockchain: Blockchain with NG, pos: PoSSelector)(updateDelay: Long => Long = identity,
                                                                                             updateBT: Long => Long = identity,
                                                                                             updateGS: ByteStr => ByteStr = identity): Block = {
    val height       = blockchain.height
    val lastBlock    = blockchain.lastBlock.get
    val ggParentTS   = blockchain.blockAt(height - 2).map(_.timestamp)
    val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
    val delay = updateDelay(
      pos
        .getValidBlockDelay(
          height,
          miner,
          lastBlock.consensusData.baseTarget,
          minerBalance
        )
        .explicitGet()
    )

    val cData = pos
      .consensusData(
        miner,
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
      .buildAndSign(3: Byte, lastBlock.timestamp + delay, lastBlock.uniqueId, updatedCData, Seq.empty, miner, Set.empty, 0.toByte)
      .explicitGet()
  }

  val accountGen: Gen[KeyPair] =
    Gen
      .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
      .map(seed => KeyPair(seed: ByteStr))

  def chainGen(balances: List[Long], blockCount: Int)(t: Time): Gen[(Seq[KeyPair], Seq[Block])] = {
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

  def calcDelay(blockForHit: Block, prevBT: Long, minerPK: PublicKey, effBalance: Long): Long = {

    val gs =
      PoSCalculator
        .generatorSignature(
          blockForHit.consensusData.generationSignature.arr,
          minerPK
        )

    val hit = PoSCalculator.hit(gs)

    FairPoSCalculator.calculateDelay(hit, prevBT, effBalance)
  }

}
