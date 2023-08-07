package com.wavesplatform.consensus

import java.nio.file.Files
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.RDB
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{WavesSettings, *}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.{TestHelpers, WithNewDBForEachTest, crypto}
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration.*
import scala.util.Random

class FPPoSSelectorTest extends FreeSpec with WithNewDBForEachTest with DBCacheSettings {
  import FPPoSSelectorTest.*

  private val generationSignatureMethods = Table(
    ("method", "block version", "vrf activated"),
    ("Blake2b256", Block.NgBlockVersion, false),
    ("VRF", Block.ProtoBlockVersion, true)
  )

  "block delay" - {
    "same on the same height in different forks" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT / 2, ENOUGH_AMT / 3), 110, blockVersion), vrfActivated) { case Env(_, blockchain, miners, blocks) =>
        val miner1 = miners.head
        val miner2 = miners.tail.head

        val miner1Balance = blockchain.effectiveBalance(miner1.toAddress, 0)

        val fork1 = mkFork(100, miner1, blockchain, blocks.last, blockVersion)
        val fork2 = mkFork(100, miner2, blockchain, blocks.last, blockVersion)

        val fork1Delay = {
          val blockForHit =
            fork1
              .lift(100)
              .orElse(
                blockchain
                  .blockHeader(blockchain.height + fork1.length - 100)
                  .map((_, blockchain.hitSource(blockchain.height + fork1.length - 100).get))
              )
              .getOrElse(fork1.head)

          val gs =
            if (vrfActivated) blockForHit._2.arr
            else PoSCalculator.generationSignature(blockForHit._2, miner1.publicKey)
          calcDelay(gs, fork1.head._1.header.baseTarget, miner1Balance)
        }

        val fork2Delay = {
          val blockForHit =
            fork2
              .lift(100)
              .orElse(
                blockchain
                  .blockHeader(blockchain.height + fork2.length - 100)
                  .map((_, blockchain.hitSource(blockchain.height + fork2.length - 100).get))
              )
              .getOrElse(fork2.head)

          val gs =
            if (vrfActivated) blockForHit._2.arr
            else PoSCalculator.generationSignature(blockForHit._2, miner1.publicKey)
          calcDelay(gs, fork2.head._1.header.baseTarget, miner1Balance)
        }

        fork1Delay shouldEqual fork2Delay
      }
    }
  }

  "block delay validation" - {
    "succeed when delay is correct" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner        = miners.head
        val height       = blockchain.height
        val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
        val lastBlock    = blockchain.lastBlockHeader.get
        val block        = forgeBlock(miner, blockchain, pos, blockVersion)()

        pos.validateBlockDelay(height, block.header, lastBlock.header, minerBalance) should beRight
      }
    }

    "failed when delay less than expected" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner        = miners.head
        val height       = blockchain.height
        val minerBalance = blockchain.effectiveBalance(miner.toAddress, 0)
        val lastBlock    = blockchain.lastBlockHeader.get
        val block        = forgeBlock(miner, blockchain, pos, blockVersion)(updateDelay = _ - 1)

        pos
          .validateBlockDelay(
            height,
            block.header,
            lastBlock.header,
            minerBalance
          ) should produce("less than min valid timestamp")
      }
    }
  }

  "base target validation" - {
    "succeed when BT is correct 1" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner     = miners.head
        val height    = blockchain.height
        val lastBlock = blockchain.lastBlockHeader.get
        val block     = forgeBlock(miner, blockchain, pos, blockVersion)()

        pos
          .validateBaseTarget(
            height + 1,
            block,
            lastBlock.header,
            blockchain.blockHeader(height - 2).map(_.header)
          ) shouldBe Right(())
      }
    }

    "failed when BT less than expected" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner     = miners.head
        val height    = blockchain.height
        val lastBlock = blockchain.lastBlockHeader.get.header
        val block     = forgeBlock(miner, blockchain, pos, blockVersion)(updateBT = _ - 1)

        pos
          .validateBaseTarget(
            height + 1,
            block,
            lastBlock,
            blockchain.blockHeader(height - 2).map(_.header)
          ) should produce("does not match calculated baseTarget")
      }
    }

    "failed when BT greater than expected" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner     = miners.head
        val height    = blockchain.height
        val lastBlock = blockchain.lastBlockHeader.get
        val block     = forgeBlock(miner, blockchain, pos, blockVersion)(updateBT = _ + 1)

        pos
          .validateBaseTarget(
            height + 1,
            block,
            lastBlock.header,
            blockchain.blockHeader(height - 2).map(_.header)
          ) should produce("does not match calculated baseTarget")
      }
    }
  }

  "generation signature validation" - {
    "succeed when GS is correct" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 10, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner = miners.head
        val block = forgeBlock(miner, blockchain, pos, blockVersion)()

        pos
          .validateGenerationSignature(block)
          .isRight shouldBe true
      }
    }

    "failed when GS is incorrect" in forAll(generationSignatureMethods) { case (_, blockVersion: Byte, vrfActivated: Boolean) =>
      withEnv(chainGen(List(ENOUGH_AMT), 100, blockVersion), vrfActivated) { case Env(pos, blockchain, miners, _) =>
        val miner = miners.head
        val block = forgeBlock(miner, blockchain, pos, blockVersion)(updateGS = gs => ByteStr(gs.arr |< Random.nextBytes))

        pos
          .validateGenerationSignature(
            block
          ) should (if (!vrfActivated) produce("Generation signatures does not match") else produce("Could not verify VRF proof"))
      }
    }
  }

  "old calculator" - {
    "delay" in {
      FairPoSCalculator.V1.calculateDelay(BigInt(1), 100L, 10000000000000L) shouldBe 705491
      FairPoSCalculator.V1.calculateDelay(BigInt(2), 200L, 20000000000000L) shouldBe 607358
      FairPoSCalculator.V1.calculateDelay(BigInt(3), 300L, 30000000000000L) shouldBe 549956
    }

    "base target" in {
      FairPoSCalculator.V1.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(99000L), 100000L) shouldBe 99L
      FairPoSCalculator.V1.calculateBaseTarget(100L, 10, 100L, 100000000000L, None, 100000000000L) shouldBe 100L
      FairPoSCalculator.V1.calculateBaseTarget(100L, 10, 100L, 100000000000L, Some(99999700000L), 100000000000L) shouldBe 100L
      FairPoSCalculator.V1.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(1L), 1000000L) shouldBe 101L
    }
  }

  "new calculator" - {
    "delay" in {
      FairPoSCalculator.V2.calculateDelay(BigInt(1), 100L, 10000000000000L) shouldBe 715491
      FairPoSCalculator.V2.calculateDelay(BigInt(2), 200L, 20000000000000L) shouldBe 617358
      FairPoSCalculator.V2.calculateDelay(BigInt(3), 300L, 30000000000000L) shouldBe 559956
    }

    "base target" in {
      FairPoSCalculator.V2.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(99000L), 100000L) shouldBe 99L
      FairPoSCalculator.V2.calculateBaseTarget(100L, 10, 100L, 100000000000L, None, 100000000000L) shouldBe 100L
      FairPoSCalculator.V2.calculateBaseTarget(100L, 10, 100L, 100000000000L, Some(99999700000L), 100000000000L) shouldBe 100L
      FairPoSCalculator.V2.calculateBaseTarget(100L, 30, 100L, 100000000000L, Some(1L), 1000000L) shouldBe 101L
    }
  }

  "PoSSelector should verify generation signature for new blocks which reference non-last block correctly" in {
    Seq(1, 100).foreach { blockCount =>
      withEnv(chainGen(List(ENOUGH_AMT, ENOUGH_AMT), blockCount, Block.ProtoBlockVersion), VRFActivated = true) {
        case Env(pos, blockchain, miners, _) =>
          val currentMiner = miners.head
          val anotherMiner = miners(1)

          val blockToApply = forgeBlock(currentMiner, blockchain, pos, Block.ProtoBlockVersion)()
          val anotherBlock = forgeBlock(anotherMiner, blockchain, pos, Block.ProtoBlockVersion)()

          blockToApply.header.reference shouldBe anotherBlock.header.reference

          blockchain.processBlock(
            blockToApply,
            crypto.verifyVRF(blockToApply.header.generationSignature, blockchain.hitSource(blockCount + 1).get.arr, blockToApply.sender).explicitGet()
          ) should beRight

          blockchain.lastBlockId shouldBe Some(blockToApply.id())

          pos.validateGenerationSignature(anotherBlock) should beRight
      }
    }

    withEnv(chainGen(List(ENOUGH_AMT, ENOUGH_AMT), 1)) { case Env(pos, blockchain, miners, _) =>
      val currentMiner = miners.head
      val anotherMiner = miners(1)

      val blockToApply = forgeBlock(currentMiner, blockchain, pos)()
      val anotherBlock = forgeBlock(anotherMiner, blockchain, pos)()

      blockToApply.header.reference shouldBe anotherBlock.header.reference

      blockchain.processBlock(
        blockToApply,
        blockchain.blockHeader(2).get.header.generationSignature
      ) should beRight

      blockchain.lastBlockId shouldBe Some(blockToApply.id())

      pos.validateGenerationSignature(anotherBlock) should beRight
    }
  }

  def withEnv(gen: Time => Gen[(Seq[KeyPair], Seq[Block])], VRFActivated: Boolean = false)(f: Env => Unit): Unit = {
    // we are not using the db instance from WithDB trait as it should be recreated between property checks
    val path = Files.createTempDirectory("lvl").toAbsolutePath
    val rdb  = RDB.open(dbSettings.copy(directory = path.toAbsolutePath.toString))
    val defaultWriter = TestRocksDB.withFunctionalitySettings(
      rdb,
      TestFunctionalitySettings.Stub.copy(preActivatedFeatures =
        Map(BlockchainFeatures.FairPoS.id -> 0) ++ (if (VRFActivated) Map(BlockchainFeatures.BlockV5.id -> 0) else Map())
      )
    )
    val settings0 = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val settings  = settings0.copy(featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))
    val bcu =
      new BlockchainUpdaterImpl(defaultWriter, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    val pos = PoSSelector(bcu, settings.synchronizationSettings.maxBaseTarget)
    try {
      val (accounts, blocks) = gen(ntpTime).sample.get

      blocks.foreach { block =>
        bcu.processBlock(block, block.header.generationSignature.take(Block.HitSourceLength)) should beRight
      }

      f(Env(pos, bcu, accounts, blocks))
      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }
}

object FPPoSSelectorTest {

  // noinspection ScalaStyle
  implicit class KComb[A](a: A) {
    def |<(f: A => Unit): A = {
      f(a)
      a
    }
  }

  final case class Env(pos: PoSSelector, blockchain: Blockchain & BlockchainUpdater, miners: Seq[KeyPair], blocks: Seq[Block])

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def mkFork(
      blockCount: Int,
      miner: KeyPair,
      blockchain: Blockchain,
      lastBlock: Block,
      blockVersion: Byte = Block.RewardBlockVersion
  ): List[(Block, ByteStr)] = {
    val height = blockchain.height

    val lastBlockHitSource = blockchain.hitSource(height).get

    ((1 to blockCount) foldLeft List((lastBlock, lastBlockHitSource))) { (forkChain, ind) =>
      val blockForHit =
        forkChain
          .lift(100)
          .orElse(blockchain.blockHeader(height + ind - 100).map((_, blockchain.hitSource(height + ind - 100).get)))
          .getOrElse(forkChain.head)

      val (gs, hitSource) =
        if (blockVersion < Block.ProtoBlockVersion) {
          val gs = ByteStr(
            PoSCalculator
              .generationSignature(
                blockForHit._2,
                miner.publicKey
              )
          )
          (gs, gs)
        } else {
          val gs = PoSCalculator
            .generationVRFSignature(
              blockForHit._2.arr,
              miner.privateKey
            )
          val gi = crypto.verifyVRF(gs, blockForHit._2.arr, miner.publicKey).explicitGet()
          (gs, gi)
        }

      val delay: Long = 60000

      val bt = FairPoSCalculator.V2.calculateBaseTarget(
        60,
        height + ind - 1,
        forkChain.head._1.header.baseTarget,
        forkChain.head._1.header.timestamp,
        (forkChain.lift(2).map(_._1.header) orElse blockchain.blockHeader(height + ind - 3).map(_.header)) map (_.timestamp),
        forkChain.head._1.header.timestamp + delay
      )

      val newBlock = Block
        .buildAndSign(
          blockVersion,
          forkChain.head._1.header.timestamp + delay,
          forkChain.head._1.id(),
          bt,
          gs,
          Seq.empty,
          miner,
          Seq.empty,
          -1L,
          None,
          None
        )
        .explicitGet()

      (newBlock, hitSource) :: forkChain
    }
  }

  def forgeBlock(miner: KeyPair, blockchain: Blockchain, pos: PoSSelector, blockVersion: Byte = Block.NgBlockVersion)(
      updateDelay: Long => Long = identity,
      updateBT: Long => Long = identity,
      updateGS: ByteStr => ByteStr = identity
  ): Block = {
    val height          = blockchain.height
    val lastBlockHeader = blockchain.lastBlockHeader.get
    val ggParentTS      = blockchain.blockHeader(height - 2).map(_.header.timestamp)
    val minerBalance    = blockchain.effectiveBalance(miner.toAddress, 0)
    val delay = updateDelay(
      pos
        .getValidBlockDelay(
          height,
          miner,
          lastBlockHeader.header.baseTarget,
          minerBalance
        )
        .explicitGet()
    )

    val cData = pos
      .consensusData(
        miner,
        height,
        60.seconds,
        lastBlockHeader.header.baseTarget,
        lastBlockHeader.header.timestamp,
        ggParentTS,
        lastBlockHeader.header.timestamp + delay
      )
      .explicitGet()

    Block
      .buildAndSign(
        blockVersion,
        lastBlockHeader.header.timestamp + delay,
        lastBlockHeader.id(),
        updateBT(cData.baseTarget),
        updateGS(cData.generationSignature),
        Seq.empty,
        miner,
        Seq.empty,
        0.toByte,
        None,
        None
      )
      .explicitGet()
  }

  val accountGen: Gen[KeyPair] =
    Gen
      .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
      .map(seed => KeyPair(ByteStr(seed)))

  def chainGen(balances: List[Long], blockCount: Int, blockVersion: Byte = Block.PlainBlockVersion)(t: Time): Gen[(Seq[KeyPair], Seq[Block])] = {
    val ts = t.correctedTime()

    Gen
      .listOfN(balances.length, accountGen)
      .map(_ zip balances)
      .map { accountsWithBalances =>
        for {
          (acc, balance) <- accountsWithBalances
          i = accountsWithBalances.indexOf((acc, balance))
        } yield (acc, GenesisTransaction.create(acc.toAddress, balance, ts + i).explicitGet())
      }
      .map { txs =>
        val lastTxTimestamp = txs.lastOption.fold(ts)(_._2.timestamp)
        val genesisBlock    = TestBlock.create(lastTxTimestamp + 1, txs.map(_._2)).block

        val chain = (1 to blockCount foldLeft List(genesisBlock)) { (blocks, d) =>
          val newBlock = TestBlock
            .create(
              lastTxTimestamp + 1 + d,
              blocks.head.id(),
              Seq.empty,
              version = blockVersion
            )
            .block
          newBlock :: blocks
        }

        (txs.map(_._1), chain.reverse)
      }
  }

  def calcDelay(gs: Array[Byte], prevBT: Long, effBalance: Long): Long = {
    val hit = PoSCalculator.hit(gs)
    FairPoSCalculator.V2.calculateDelay(hit, prevBT, effBalance)
  }
}
