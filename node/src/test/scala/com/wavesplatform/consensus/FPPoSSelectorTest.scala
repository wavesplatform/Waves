package com.wavesplatform.consensus

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBFactory
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{WavesSettings, _}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils.TestLevelDB
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{TestHelpers, TransactionGen, WithDB, crypto}
import org.iq80.leveldb.Options
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class FPPoSSelectorTest extends FreeSpec with Matchers with WithDB with TransactionGen with DBCacheSettings with ScalaCheckPropertyChecks {
  import FPPoSSelectorTest._

  val generationSignatureMethods = Table(
    ("method", "block version", "vrf activated"),
    ("Blake2b256", Block.NgBlockVersion, false),
    ("VRF", Block.ProtoBlockVersion, true)
  )

  "block delay" - {
    "same on the same height in different forks" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT / 2, ENOUGH_AMT / 3), 110, blockVersion), vrfActivated) {
          case Env(_, blockchain, miners) =>
            val miner1 = miners.head
            val miner2 = miners.tail.head

            val miner1Balance = blockchain.effectiveBalance(miner1.toAddress, 0)

            val fork1 = mkFork(100, miner1, blockchain, blockVersion)
            val fork2 = mkFork(100, miner2, blockchain, blockVersion)

            val fork1Delay = {
              val blockForHit =
                fork1
                  .lift(100)
                  .orElse(
                    blockchain
                      .blockAt(blockchain.height + fork1.length - 100)
                      .map((_, blockchain.hitSourceAtHeight(blockchain.height + fork1.length - 100).get))
                  )
                  .getOrElse(fork1.head)

              val gs =
                if (vrfActivated)
                  blockForHit._2.arr
                else
                  PoSCalculator
                    .generationSignature(
                      blockForHit._2.arr,
                      miner1
                    )
              calcDelay(gs, fork1.head._1.header.baseTarget, miner1Balance)
            }

            val fork2Delay = {
              val blockForHit =
                fork2
                  .lift(100)
                  .orElse(
                    blockchain
                      .blockAt(blockchain.height + fork2.length - 100)
                      .map((_, blockchain.hitSourceAtHeight(blockchain.height + fork2.length - 100).get))
                  )
                  .getOrElse(fork2.head)

              val gs =
                if (vrfActivated)
                  blockForHit._2.arr
                else
                  PoSCalculator
                    .generationSignature(
                      blockForHit._2.arr,
                      miner1
                    )
              calcDelay(gs, fork2.head._1.header.baseTarget, miner1Balance)
            }

            fork1Delay shouldEqual fork2Delay
        }
    }
  }

  "block delay validation" - {
    "succeed when delay is correct" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner        = miners.head
            val height       = blockchain.height
            val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
            val lastBlock    = blockchain.lastBlock.get
            val block        = forgeBlock(miner, blockchain, pos, blockVersion)()

            pos
              .validateBlockDelay(height + 1, block, lastBlock.header, minerBalance)
              .explicitGet()
        }
    }

    "failed when delay less than expected" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner        = miners.head
            val height       = blockchain.height
            val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
            val lastBlock    = blockchain.lastBlock.get
            val block        = forgeBlock(miner, blockchain, pos, blockVersion)(updateDelay = _ - 1)

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
    "succeed when BT is correct 1" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner     = miners.head
            val height    = blockchain.height
            val lastBlock = blockchain.lastBlock.get
            val block     = forgeBlock(miner, blockchain, pos, blockVersion)()

            pos
              .validateBaseTarget(
                height + 1,
                block,
                lastBlock.header,
                blockchain.blockAt(height - 2).map(_.header)
              ) shouldBe Right(())
        }
    }

    "failed when BT less than expected" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner     = miners.head
            val height    = blockchain.height
            val lastBlock = blockchain.lastBlock.get
            val block     = forgeBlock(miner, blockchain, pos, blockVersion)(updateBT = _ - 1)

            pos
              .validateBaseTarget(
                height + 1,
                block,
                lastBlock.header,
                blockchain.blockAt(height - 2).map(_.header)
              ) should produce("does not match calculated baseTarget")
        }
    }

    "failed when BT greater than expected" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner     = miners.head
            val height    = blockchain.height
            val lastBlock = blockchain.lastBlock.get
            val block     = forgeBlock(miner, blockchain, pos, blockVersion)(updateBT = _ + 1)

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
    "succeed when GS is correct" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner  = miners.head
            val height = blockchain.height
            val block  = forgeBlock(miner, blockchain, pos, blockVersion)()

            pos
              .validateGenerationSignature(
                height + 1,
                block
              )
              .isRight shouldBe true
        }
    }

    "failed when GS is incorrect" in forAll(generationSignatureMethods) {
      case (_, blockVersion: Byte, vrfActivated: Boolean) =>
        withEnv(chainGen(List(ENOUGH_AMT), 100, blockVersion), vrfActivated) {
          case Env(pos, blockchain, miners) =>
            val miner  = miners.head
            val height = blockchain.height
            val block  = forgeBlock(miner, blockchain, pos, blockVersion)(updateGS = gs => ByteStr(gs.arr |< Random.nextBytes))

            pos
              .validateGenerationSignature(
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
    // we are not using the db instance from WithDB trait as it should be recreated between property checks
    val path = Files.createTempDirectory("lvl").toAbsolutePath
    val db   = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
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
        bcu.processBlock(block, block.header.generationSignature.take(Block.GenerationInputLength)).explicitGet()
      }

      f(Env(pos, bcu, accounts))
      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
      TestHelpers.deleteRecursively(path)
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

  def mkFork(blockCount: Int, miner: KeyPair, blockchain: Blockchain, blockVersion: Byte = Block.RewardBlockVersion): List[(Block, ByteStr)] = {
    val height = blockchain.height

    val lastBlock                = blockchain.lastBlock.get
    val lastBlockGenerationInput = blockchain.hitSourceAtHeight(height).get

    ((1 to blockCount) foldLeft List((lastBlock, lastBlockGenerationInput))) { (forkChain, ind) =>
      val blockForHit =
        forkChain
          .lift(100)
          .orElse(blockchain.blockAt(height + ind - 100).map((_, blockchain.hitSourceAtHeight(height + ind - 100).get)))
          .getOrElse(forkChain.head)

      val (gs, generationInput) =
        if (blockVersion < Block.ProtoBlockVersion) {
          val gs = PoSCalculator
            .generationSignature(
              blockForHit._2.arr,
              miner.publicKey
            )
          (gs, gs)
        } else {
          val gs = PoSCalculator
            .generationVRFSignature(
              blockForHit._2.arr,
              miner.privateKey
            )
          val gi = crypto.verifyVRF(ByteStr(gs), blockForHit._2, miner.publicKey).explicitGet().arr
          (gs, gi)
        }

      val delay: Long = 60000

      val bt = FairPoSCalculator.calculateBaseTarget(
        60,
        height + ind - 1,
        forkChain.head._1.header.baseTarget,
        forkChain.head._1.header.timestamp,
        (forkChain.lift(2).map(_._1) orElse blockchain.blockAt(height + ind - 3)) map (_.header.timestamp),
        forkChain.head._1.header.timestamp + delay
      )

      val newBlock = Block
        .buildAndSign(
          blockVersion,
          forkChain.head._1.header.timestamp + delay,
          forkChain.head._1.uniqueId,
          bt,
          ByteStr(gs),
          Seq.empty,
          miner,
          Set.empty,
          -1L
        )
        .explicitGet()

      (newBlock, ByteStr(generationInput)) :: forkChain
    }
  }

  def forgeBlock(
      miner: KeyPair,
      blockchain: Blockchain with NG,
      pos: PoSSelector,
      blockVersion: Byte = Block.NgBlockVersion
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
        blockVersion,
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

  def calcDelay(gs: Array[Byte], prevBT: Long, effBalance: Long): Long = {
    val hit = PoSCalculator.hit(gs)
    FairPoSCalculator.calculateDelay(hit, prevBT, effBalance)
  }
}
