package com.wavesplatform.consensus

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{WavesSettings, _}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{TransactionGen, WithDB, crypto}
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

            calcDelay(blockForHit, fork1.head.header.baseTarget, miner1, miner1Balance)
          }

          val fork2Delay = {
            val blockForHit =
              fork2
                .lift(100)
                .orElse(blockchain.blockAt(blockchain.height + fork2.length - 100))
                .getOrElse(fork2.head)

            calcDelay(blockForHit, fork2.head.header.baseTarget, miner1, miner1Balance)
          }

          fork1Delay shouldEqual fork2Delay
      }
    }

    "same on the same height in different forks (VRF)" ignore {
      withEnv(chainGen(List(ENOUGH_AMT / 2, ENOUGH_AMT / 3), 110, Block.ProtoBlockVersion)) {
        case Env(_, blockchain, miners) =>
          val miner1 = miners.head
          val miner2 = miners.tail.head

          val miner1Balance = blockchain.effectiveBalance(miner1.toAddress, 0)

          val fork1 = mkFork(10, miner1, blockchain, Block.ProtoBlockVersion)
          val fork2 = mkFork(10, miner2, blockchain, Block.ProtoBlockVersion)

          val fork1Delay = {
            val blockForHit =
              fork1
                .lift(100)
                .orElse(blockchain.blockAt(blockchain.height + fork1.length - 100))
                .getOrElse(fork1.head)

            val refBlock = fork1.find(b => b.uniqueId == blockForHit.header.reference).get

            calcVRFDelay(blockForHit, fork1.head.header.baseTarget, refBlock.header.generationSignature, miner1Balance)
          }

          val fork2Delay = {
            val blockForHit =
              fork2
                .lift(100)
                .orElse(blockchain.blockAt(blockchain.height + fork2.length - 100))
                .getOrElse(fork2.head)

            val refBlock = fork2.find(b => b.uniqueId == blockForHit.header.reference).get

            calcVRFDelay(blockForHit, fork2.head.header.baseTarget, refBlock.header.generationSignature, miner1Balance)
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
            .validateBlockDelay(height + 1, block, lastBlock.header, minerBalance)
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
              lastBlock.header,
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
              lastBlock.header,
              blockchain.blockAt(height - 2).map(_.header)
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
              lastBlock.header,
              blockchain.blockAt(height - 2).map(_.header)
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
              lastBlock.header,
              blockchain.blockAt(height - 2).map(_.header)
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
            ).isRight shouldBe true
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
      FairPoSCalculator.calculateDelay(BigInt(1), 100L, 10000000000000L) shouldBe 705491
      FairPoSCalculator.calculateDelay(BigInt(2), 200L, 20000000000000L) shouldBe 607358
      FairPoSCalculator.calculateDelay(BigInt(3), 300L, 30000000000000L) shouldBe 549956
    }

    "base target" in {
      FairPoSCalculator.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(99000L), 100000L) shouldBe 99L
      FairPoSCalculator.calculateBaseTarget(100L, 10, 100L, 100000000000L, None, 100000000000L) shouldBe 100L
      FairPoSCalculator.calculateBaseTarget(100L, 10, 100L, 100000000000L, Some(99999700000L), 100000000000L) shouldBe 100L
      FairPoSCalculator.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(1L), 1000000L) shouldBe 101L
    }
  }

  def withEnv(gen: Time => Gen[(Seq[KeyPair], Seq[Block])], VRFActivated: Boolean = false)(f: Env => Unit): Unit = {
    val defaultWriter = TestLevelDB.withFunctionalitySettings(
      db,
      ignoreSpendableBalanceChanged,
      TestFunctionalitySettings.Stub.copy(
        preActivatedFeatures = Map(BlockchainFeatures.FairPoS.id -> 0) ++ (if (VRFActivated) Map(BlockchainFeatures.BlockV5.id -> 0) else Map())
      ),
      dbSettings
    )
    val settings0 = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings  = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu       = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime, ignoreBlockchainUpdated)
    val pos       = new PoSSelector(bcu, settings.blockchainSettings, settings.synchronizationSettings)
    try {
      val (accounts, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block, block.header.generationSignature).explicitGet()
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

  //noinspection ScalaStyle
  implicit class KComb[A](a: A) {
    def |<(f: A => Unit): A = {
      f(a)
      a
    }
  }

  final case class Env(pos: PoSSelector, blockchain: BlockchainUpdater with NG, miners: Seq[KeyPair])

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def mkFork(blockCount: Int, miner: KeyPair, blockchain: Blockchain, blockVersion: Byte = Block.RewardBlockVersion): List[Block] = {
    val height = blockchain.height

    val lastBlock = blockchain.lastBlock.get

    ((1 to blockCount) foldLeft List(lastBlock)) { (forkChain, ind) =>
      val blockForHit =
        forkChain
          .lift(100)
          .orElse(blockchain.blockAt(height + ind - 100))
          .getOrElse(forkChain.head)

      val gs =
        if (blockVersion < Block.ProtoBlockVersion)
          PoSCalculator
            .generationSignature(
              blockForHit.header.generationSignature.arr,
              miner
            )
        else
          PoSCalculator
            .generationVRFSignature(
              blockForHit.header.generationSignature.arr,
              miner
            )

      val delay: Long = 60000

      val bt = FairPoSCalculator.calculateBaseTarget(
        60,
        height + ind - 1,
        forkChain.head.header.baseTarget,
        forkChain.head.header.timestamp,
        (forkChain.lift(2) orElse blockchain.blockAt(height + ind - 3)) map (_.header.timestamp),
        forkChain.head.header.timestamp + delay
      )

      val newBlock = Block
        .buildAndSign(
          blockVersion: Byte,
          forkChain.head.header.timestamp + delay,
          forkChain.head.uniqueId,
          bt,
          ByteStr(gs),
          Seq.empty,
          miner,
          Set.empty,
          -1L
        )
        .explicitGet()

      newBlock :: forkChain
    }
  }

  def forgeBlock(
      miner: KeyPair,
      blockchain: Blockchain with NG,
      pos: PoSSelector
  )(updateDelay: Long => Long = identity, updateBT: Long => Long = identity, updateGS: ByteStr => ByteStr = identity): Block = {
    val height       = blockchain.height
    val lastBlock    = blockchain.lastBlock.get
    val ggParentTS   = blockchain.blockAt(height - 2).map(_.header.timestamp)
    val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
    val delay = updateDelay(
      pos
        .getValidBlockDelay(
          height,
          miner,
          lastBlock.header.baseTarget,
          minerBalance
        )
        .explicitGet()
    )

    val cData = pos
      .consensusData(
        miner,
        height,
        60.seconds,
        lastBlock.header.baseTarget,
        lastBlock.header.timestamp,
        ggParentTS,
        lastBlock.header.timestamp + delay
      )
      .explicitGet()

    Block
      .buildAndSign(
        3: Byte,
        lastBlock.header.timestamp + delay,
        lastBlock.uniqueId,
        updateBT(cData.baseTarget),
        updateGS(cData.generationSignature),
        Seq.empty,
        miner,
        Set.empty,
        0.toByte
      )
      .explicitGet()
  }

  val accountGen: Gen[KeyPair] =
    Gen
      .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
      .map(seed => KeyPair(seed: ByteStr))

  def chainGen(balances: List[Long], blockCount: Int, blockVersion: Byte = Block.PlainBlockVersion)(t: Time): Gen[(Seq[KeyPair], Seq[Block])] = {
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
              Seq.empty,
              version = blockVersion
            )
          newBlock :: blocks
        }

        (txs.map(_._1), chain.reverse)
      }
  }

  def calcDelay(blockForHit: Block, prevBT: Long, minerPK: PublicKey, effBalance: Long): Long = {

    val gs =
      PoSCalculator
        .generationSignature(
          blockForHit.header.generationSignature.arr,
          minerPK
        )

    val hit = PoSCalculator.hit(gs)

    FairPoSCalculator.calculateDelay(hit, prevBT, effBalance)
  }

  def calcVRFDelay(blockForHit: Block, prevBT: Long, refGS: ByteStr, effBalance: Long): Long = {

    val gs = crypto.verifyVRF(blockForHit.header.generationSignature, refGS, blockForHit.header.generator).right.get.arr

    val hit = PoSCalculator.hit(gs)

    FairPoSCalculator.calculateDelay(hit, prevBT, effBalance)
  }

}
