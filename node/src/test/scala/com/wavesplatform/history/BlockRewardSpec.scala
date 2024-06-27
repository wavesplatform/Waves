package com.wavesplatform.history

import cats.syntax.option.*
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.http.RewardApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.Keys
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockReward, BlockRewardDistribution, ConsensusImprovements}
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, RewardsSettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{BlockRewardCalculator, Blockchain, Height}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{RideV6, WavesSettingsOps, BlockRewardDistribution as BlockRewardDistributionSettings}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import org.scalacheck.Gen
import org.scalactic.source.Position

class BlockRewardSpec extends FreeSpec with WithDomain {

  private val BlockRewardActivationHeight = 5
  private val NGActivationHeight          = 0
  private val InitialReward               = 6 * Constants.UnitsInWave
  private val rewardSettings = settings.copy(
    blockchainSettings = DefaultBlockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockReward.id    -> BlockRewardActivationHeight,
          BlockchainFeatures.NG.id             -> NGActivationHeight,
          BlockchainFeatures.FeeSponsorship.id -> -10
        ),
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue
      ),
      rewardsSettings = RewardsSettings(
        10,
        5,
        InitialReward,
        1 * Constants.UnitsInWave,
        4
      )
    )
  )

  private def mkEmptyBlock(ref: ByteStr, signer: KeyPair): Block = TestBlock.create(ntpNow, ref, Seq.empty, signer).block

  private def mkEmptyBlockIncReward(ref: ByteStr, signer: KeyPair): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = InitialReward + 1 * Constants.UnitsInWave, version = Block.RewardBlockVersion).block

  private def mkEmptyBlockDecReward(ref: ByteStr, signer: KeyPair): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = InitialReward - 1 * Constants.UnitsInWave, version = Block.RewardBlockVersion).block

  private def mkEmptyBlockReward(ref: ByteStr, signer: KeyPair, vote: Long): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = vote, version = Block.RewardBlockVersion).block

  private val InitialMinerBalance = 10000 * Constants.UnitsInWave
  private val OneTotalFee         = 100000
  private val OneCarryFee         = (OneTotalFee * 0.6).toLong
  private val OneFee              = (OneTotalFee * 0.4).toLong

  private val genesis = for {
    sourceAddress <- accountGen
    issuer        <- accountGen
    miner1        <- accountGen
    miner2        <- accountGen
    genesisBlock = TestBlock
      .create(
        ntpTime.getTimestamp(),
        Seq(
          GenesisTransaction
            .create(sourceAddress.toAddress, (Constants.TotalWaves - 60000) * Constants.UnitsInWave, ntpTime.getTimestamp())
            .explicitGet(),
          GenesisTransaction.create(issuer.toAddress, 40000 * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(miner1.toAddress, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
          GenesisTransaction.create(miner2.toAddress, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet()
        )
      )
      .block

  } yield (sourceAddress, issuer, miner1, miner2, genesisBlock)

  private val activationScenario = for {
    (sourceAddress, _, miner, _, genesisBlock) <- genesis
    recipient                                  <- accountGen
    transfers <- Gen.listOfN(10, transferGeneratorP(ntpNow, sourceAddress, recipient.toAddress, 1000 * Constants.UnitsInWave))
    b2              = TestBlock.create(ntpNow, genesisBlock.id(), transfers, miner).block
    b3              = mkEmptyBlock(b2.id(), miner)
    b4              = mkEmptyBlock(b3.id(), miner)
    b5              = mkEmptyBlock(b4.id(), miner)
    b6              = mkEmptyBlock(b5.id(), miner)
    b7              = mkEmptyBlock(b6.id(), miner)
    b8              = mkEmptyBlock(b7.id(), miner)
    b9              = mkEmptyBlock(b8.id(), miner)
    b10             = mkEmptyBlock(b9.id(), miner)
    b11             = mkEmptyBlockIncReward(b10.id(), miner)
    b12             = mkEmptyBlockIncReward(b11.id(), miner)
    b13             = mkEmptyBlockIncReward(b12.id(), miner)
    b14             = mkEmptyBlockIncReward(b13.id(), miner)
    b15             = mkEmptyBlockIncReward(b14.id(), miner)
    secondTermStart = BlockRewardActivationHeight + 10
    b16 = Range
      .inclusive(secondTermStart + 1, secondTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b15)) {
        case (prev, i)
            if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i, modifyTerm = false).contains(i) =>
          prev :+ mkEmptyBlockDecReward(prev.last.id(), miner)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.id(), miner)
      }
      .tail
    thirdTermStart = BlockRewardActivationHeight + 10 + 10
    b17 = Range
      .inclusive(thirdTermStart + 1, thirdTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b16.last)) {
        case (prev, i)
            if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i, modifyTerm = false).contains(i) =>
          prev :+ mkEmptyBlockReward(prev.last.id(), miner, -1L)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.id(), miner)
      }
      .tail
    fourthTermStart = BlockRewardActivationHeight + 10 + 10 + 10
    b18 = Range
      .inclusive(fourthTermStart + 1, fourthTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b17.last)) {
        case (prev, i)
            if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i, modifyTerm = false).contains(i) =>
          prev :+ mkEmptyBlockReward(prev.last.id(), miner, 0)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.id(), miner)
      }
      .tail
  } yield (miner, transfers, Seq(genesisBlock, b2), Seq(b3, b4), b5, Seq(b6, b7, b8, b9), Seq(b10, b11, b12, b13, b14), b15, b16, b17, b18)

  "Miner receives reward as soon as the feature is activated and changes reward amount after voting" in forAll(activationScenario) {
    case (miner, transfers, b1s, b2s, activationBlock, b3s, b4s, newTermBlock, b5s, b6s, b7s) =>
      withDomain(rewardSettings) { d =>
        val totalFee = transfers.map(_.fee.value).sum

        b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)

        b2s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe false
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight - 1) shouldBe None
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(activationBlock) should beRight
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight) shouldBe Some(InitialReward)
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialReward + InitialMinerBalance + totalFee

        b3s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 4
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 4) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 5 * InitialReward + InitialMinerBalance + totalFee

        b4s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 9
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 9) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + InitialMinerBalance + totalFee

        val NextReward = InitialReward + 1 * Constants.UnitsInWave

        d.blockchainUpdater.processBlock(newTermBlock) should beRight
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + NextReward + InitialMinerBalance + totalFee
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10) shouldBe NextReward.some

        b5s.init.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 - 1) shouldBe NextReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + 10 * NextReward + InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(b5s.last) should beRight

        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + 10 * NextReward + InitialReward + InitialMinerBalance + totalFee

        b6s.init.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 + 10 - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 + 10 - 1) shouldBe InitialReward.some
        d.blockchainUpdater.balance(
          miner.toAddress
        ) shouldBe 10 * InitialReward + 10 * NextReward + 10 * InitialReward + InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(b6s.last) should beRight

        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 + 10) shouldBe InitialReward.some
        d.blockchainUpdater.balance(
          miner.toAddress
        ) shouldBe 10 * InitialReward + 10 * NextReward + 11 * InitialReward + InitialMinerBalance + totalFee

        b7s.init.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 + 10 + 10 - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 + 10 + 10 - 1) shouldBe InitialReward.some
        d.blockchainUpdater.balance(
          miner.toAddress
        ) shouldBe 10 * InitialReward + 10 * NextReward + 20 * InitialReward + InitialMinerBalance + totalFee

        val DecreasedReward = InitialReward - 1 * Constants.UnitsInWave

        d.blockchainUpdater.processBlock(b7s.last) should beRight

        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 + 10 + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 + 10 + 10) shouldBe DecreasedReward.some
        d.blockchainUpdater.balance(
          miner.toAddress
        ) shouldBe 10 * InitialReward + 10 * NextReward + 20 * InitialReward + DecreasedReward + InitialMinerBalance + totalFee
      }
  }

  "Miner receives reward and fees" - {
    val ngEmptyScenario = for {
      (sourceAddress, issuer, miner1, miner2, genesisBlock) <- genesis
      tx1 = TransferTransaction
        .selfSigned(
          1.toByte,
          issuer,
          sourceAddress.toAddress,
          Waves,
          10 * Constants.UnitsInWave,
          Waves,
          OneTotalFee,
          ByteStr.empty,
          ntpTime.getTimestamp()
        )
        .explicitGet()
      tx2 = TransferTransaction
        .selfSigned(
          1.toByte,
          issuer,
          sourceAddress.toAddress,
          Waves,
          10 * Constants.UnitsInWave,
          Waves,
          OneTotalFee,
          ByteStr.empty,
          ntpTime.getTimestamp()
        )
        .explicitGet()
      b2        = mkEmptyBlock(genesisBlock.id(), miner1)
      b3        = mkEmptyBlock(b2.id(), miner1)
      b4        = TestBlock.create(ntpNow, b3.id(), Seq(tx1), miner1).block
      (b5, m5s) = chainBaseAndMicro(b4.id(), Seq.empty, Seq(Seq(tx2)), miner2, 3.toByte, ntpNow)
    } yield (miner1, miner2, Seq(genesisBlock, b2, b3, b4), b5, m5s)

    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): BlockDiffer.Result =
      BlockDiffer.fromBlock(blockchain, prevBlock, b, None, MiningConstraint.Unlimited: MiningConstraint, b.header.generationSignature).explicitGet()

    "when NG state is empty" in forAll(ngEmptyScenario) { case (miner1, miner2, b2s, b3, m3s) =>
      withDomain(rewardSettings) { d =>
        b2s.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
          val BlockDiffer.Result(snapshot, carryFee, totalFee, _, _, computedStateHash) = differ(d.rocksDBWriter, prevBlock, curBlock)
          d.rocksDBWriter.append(snapshot, carryFee, totalFee, None, curBlock.header.generationSignature, computedStateHash, curBlock)
          Some(curBlock)
        }

        d.rocksDBWriter.height shouldBe BlockRewardActivationHeight - 1
        d.rocksDBWriter.balance(miner1.toAddress) shouldBe InitialMinerBalance + OneFee
        d.rdb.db.get(Keys.blockMetaAt(Height(BlockRewardActivationHeight - 1))).map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.rocksDBWriter.carryFee(None) shouldBe OneCarryFee

        d.blockchainUpdater.processBlock(b3) should beRight
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe 0L.some
        d.blockchainUpdater.carryFee(None) shouldBe 0L

        m3s.foreach(mb => d.blockchainUpdater.processMicroBlock(mb, None) should beRight)

        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee(None) shouldBe OneCarryFee
      }
    }

    val betterBlockScenario = for {
      (sourceAddress, issuer, miner, _, genesisBlock) <- genesis
      tx = TransferTransaction
        .selfSigned(
          1.toByte,
          issuer,
          sourceAddress.toAddress,
          Waves,
          10 * Constants.UnitsInWave,
          Waves,
          OneTotalFee,
          ByteStr.empty,
          ntpTime.getTimestamp()
        )
        .explicitGet()
      b2        = mkEmptyBlock(genesisBlock.id(), miner)
      b3        = mkEmptyBlock(b2.id(), miner)
      b4        = mkEmptyBlock(b3.id(), miner)
      (b5, m5s) = chainBaseAndMicro(b4.id(), Seq.empty, Seq(Seq(tx)), miner, 3.toByte, ntpNow)
      b6a       = TestBlock.create(ntpNow, m5s.last.totalResBlockSig, Seq.empty, miner).block
      b6b = TestBlock
        .sign(
          miner,
          b6a.copy(header = b6a.header.copy(timestamp = b6a.header.timestamp - 1L))
        )
        .block
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5), m5s, b6a, b6b)

    "when received better liquid block" in forAll(betterBlockScenario) { case (miner, b1s, m1s, b2a, b2b) =>
      withDomain(rewardSettings) { d =>
        b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        m1s.foreach(m => d.blockchainUpdater.processMicroBlock(m, None) should beRight)

        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee(None) shouldBe OneCarryFee

        d.blockchainUpdater.processBlock(b2a) should beRight
        d.blockchainUpdater.processBlock(b2b) should beRight

        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + InitialReward + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe 0L.some
        d.blockchainUpdater.carryFee(None) shouldBe 0L
      }
    }
    val blockWithoutFeesScenario = for {
      (_, _, miner1, miner2, genesisBlock) <- genesis
      b2 = mkEmptyBlock(genesisBlock.id(), miner1)
      b3 = mkEmptyBlock(b2.id(), miner1)
      b4 = mkEmptyBlock(b3.id(), miner1)
      b5 = mkEmptyBlockIncReward(b4.id(), miner1)
      b6s = Range
        .inclusive(BlockRewardActivationHeight + 1, BlockRewardActivationHeight + rewardSettings.blockchainSettings.rewardsSettings.term)
        .foldLeft(Seq(b5)) {
          case (prev, i)
              if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i, modifyTerm = false).contains(i) =>
            prev :+ mkEmptyBlockIncReward(prev.last.id(), if (i % 2 == 0) miner2 else miner1)
          case (prev, i) => prev :+ mkEmptyBlock(prev.last.id(), if (i % 2 == 0) miner2 else miner1)
        }
        .tail
    } yield (miner1, miner2, Seq(genesisBlock, b2, b3, b4), b5, b6s.init, b6s.last)

    "when all blocks without fees" in forAll(blockWithoutFeesScenario) { case (miner1, miner2, b1s, b2, b3s, b4) =>
      withDomain(rewardSettings) { d =>
        val initialWavesAmount = BigInt(Constants.TotalWaves) * BigInt(Constants.UnitsInWave)
        val term               = rewardSettings.blockchainSettings.rewardsSettings.term
        val minIncrement       = rewardSettings.blockchainSettings.rewardsSettings.minIncrement
        b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight - 1
        d.blockchainUpdater.wavesAmount(BlockRewardActivationHeight - 1) shouldBe initialWavesAmount
        d.blockchainUpdater.balance(miner1.toAddress) shouldBe InitialMinerBalance
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance
        d.blockchainUpdater.processBlock(b2) should beRight
        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.wavesAmount(BlockRewardActivationHeight) shouldBe initialWavesAmount + InitialReward
        d.blockchainUpdater.balance(miner1.toAddress) shouldBe InitialMinerBalance + InitialReward
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance
        b3s.zipWithIndex.foreach { case (b, i) =>
          d.blockchainUpdater.processBlock(b) should beRight
          d.blockchainUpdater.height shouldBe BlockRewardActivationHeight + i + 1
          d.blockchainUpdater.wavesAmount(BlockRewardActivationHeight + i + 1) shouldBe initialWavesAmount + BigInt(InitialReward * (i + 2))
          d.blockchainUpdater.balance(miner1.toAddress) shouldBe InitialMinerBalance + ((i + 1) / 2) * InitialReward + InitialReward
          d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + (i / 2 + 1) * InitialReward
        }
        d.blockchainUpdater.processBlock(b4) should beRight
        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight + term
        d.blockchainUpdater.wavesAmount(
          BlockRewardActivationHeight + term
        ) shouldBe initialWavesAmount + term * InitialReward + InitialReward + minIncrement
        d.blockchainUpdater.balance(miner1.toAddress) shouldBe InitialMinerBalance + InitialReward * 5 + InitialReward + minIncrement
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward * 5
      }
    }
  }

  private val calcRewardSettings = rewardSettings.copy(
    blockchainSettings = rewardSettings.blockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockReward.id    -> 4,
          BlockchainFeatures.NG.id             -> NGActivationHeight,
          BlockchainFeatures.FeeSponsorship.id -> -10
        ),
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue
      ),
      rewardsSettings = RewardsSettings(12, 6, 6 * Constants.UnitsInWave, 1 * Constants.UnitsInWave, 6)
    )
  )

  private val calcScenario = for {
    (_, _, miner, _, genesisBlock) <- genesis
    b2  = mkEmptyBlock(genesisBlock.id(), miner)
    b3  = mkEmptyBlock(b2.id(), miner)
    b4  = mkEmptyBlock(b3.id(), miner)
    b5  = mkEmptyBlock(b4.id(), miner)
    b6  = mkEmptyBlock(b5.id(), miner)
    b7  = mkEmptyBlock(b6.id(), miner)
    b8  = mkEmptyBlock(b7.id(), miner)
    b9  = mkEmptyBlock(b8.id(), miner)
    b10 = mkEmptyBlockIncReward(b9.id(), miner)
    b11 = mkEmptyBlockIncReward(b10.id(), miner)
    b12 = mkEmptyBlockIncReward(b11.id(), miner)
    b13 = mkEmptyBlockIncReward(b12.id(), miner)
    b14 = mkEmptyBlockIncReward(b13.id(), miner)
    b15 = mkEmptyBlockIncReward(b14.id(), miner)
    b16 = mkEmptyBlockIncReward(b15.id(), miner)
  } yield (Seq(genesisBlock, b2, b3), b4, Seq(b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15), b16)

  "Reward calculated correctly" in forAll(calcScenario) { case (b1s, b2, b3s, b4) =>
    withDomain(calcRewardSettings) { d =>
      b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)

      d.blockchainUpdater.processBlock(b2)

      b3s.foreach(b => d.blockchainUpdater.processBlock(b))

      d.blockchainUpdater.height shouldBe 15

      val calcSettings = calcRewardSettings.blockchainSettings.rewardsSettings
      calcSettings.nearestTermEnd(4, 9, modifyTerm = false) shouldBe 15
      calcSettings.nearestTermEnd(4, 10, modifyTerm = false) shouldBe 15

      val route = RewardApiRoute(d.blockchainUpdater)

      d.blockchainUpdater.blockReward(9) shouldBe (6 * Constants.UnitsInWave).some
      d.blockchainUpdater.blockReward(15) shouldBe (6 * Constants.UnitsInWave).some

      d.blockchainUpdater.processBlock(b4) should beRight
      d.blockchainUpdater.blockReward(16) shouldBe (7 * Constants.UnitsInWave).some

      route.getRewards(9).explicitGet().votes.increase shouldBe 0
      route.getRewards(10).explicitGet().votes.increase shouldBe 1

    }
  }

  private val smallPeriodRewardSettings = rewardSettings.copy(
    blockchainSettings = rewardSettings.blockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockReward.id    -> 4,
          BlockchainFeatures.NG.id             -> NGActivationHeight,
          BlockchainFeatures.FeeSponsorship.id -> -10
        ),
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue
      ),
      rewardsSettings = RewardsSettings(3, 2, 6 * Constants.UnitsInWave, 1 * Constants.UnitsInWave, 2)
    )
  )

  private val smallCalcScenario = for {
    (_, _, miner, _, genesisBlock) <- genesis
    b2 = mkEmptyBlock(genesisBlock.id(), miner)
    b3 = mkEmptyBlock(b2.id(), miner)
    b4 = mkEmptyBlock(b3.id(), miner)
    b5 = mkEmptyBlockIncReward(b4.id(), miner)
    b6 = mkEmptyBlockIncReward(b5.id(), miner)
    b7 = mkEmptyBlockIncReward(b6.id(), miner)
  } yield (Seq(genesisBlock, b2, b3), b4, Seq(b5, b6, b7))

  "Reward calculated correctly for small voting period" in forAll(smallCalcScenario) { case (b1s, b2, b3s) =>
    withDomain(smallPeriodRewardSettings) { d =>
      b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)

      d.blockchainUpdater.processBlock(b2)

      b3s.foreach(b => d.blockchainUpdater.processBlock(b))

      d.blockchainUpdater.height shouldBe 7

      d.blockchainUpdater.blockReward(7) shouldBe (7 * Constants.UnitsInWave).some
    }
  }

  s"Reward for genesis block should be 0 after activation of $ConsensusImprovements" in {
    withDomain(RideV6.setFeaturesHeight(BlockReward -> 0, ConsensusImprovements -> 999)) { d =>
      val block = d.appendBlock(TxHelpers.genesis(TxHelpers.secondAddress))
      d.blockchain.balance(block.sender.toAddress) shouldBe 6_0000_0000
      d.appendBlock()
      d.blockchain.balance(block.sender.toAddress) shouldBe 12_0000_0000
    }

    withDomain(RideV6.setFeaturesHeight(BlockReward -> 0, ConsensusImprovements -> 0)) { d =>
      val block = d.appendBlock(TxHelpers.genesis(TxHelpers.secondAddress))
      d.blockchain.balance(block.sender.toAddress) shouldBe 0
      d.appendBlock()
      d.blockchain.balance(block.sender.toAddress) shouldBe 6_0000_0000
    }

    withDomain(RideV6.setFeaturesHeight(BlockReward -> 0, ConsensusImprovements -> 1)) { d =>
      val block = d.appendBlock(TxHelpers.genesis(TxHelpers.secondAddress))
      d.blockchain.balance(block.sender.toAddress) shouldBe 0
      d.appendBlock()
      d.blockchain.balance(block.sender.toAddress) shouldBe 6_0000_0000
    }

    withDomain(RideV6.setFeaturesHeight(BlockReward -> 1, ConsensusImprovements -> 1)) { d =>
      val block = d.appendBlock(TxHelpers.genesis(TxHelpers.secondAddress))
      d.blockchain.balance(block.sender.toAddress) shouldBe 0
      d.appendBlock()
      d.blockchain.balance(block.sender.toAddress) shouldBe 6_0000_0000
    }
  }

  s"Reward should be distributed between miner, daoAddress and xtnBuybackAddress after ${BlockRewardDistribution.description} activation" in {
    val daoAddress        = TxHelpers.address(101)
    val xtnBuybackAddress = TxHelpers.address(102)

    val settingsWithoutAddresses = RideV6.copy(blockchainSettings =
      RideV6.blockchainSettings.copy(functionalitySettings =
        RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = None, xtnBuybackAddress = None)
      )
    )
    val settingsWithOnlyDaoAddress = RideV6.copy(blockchainSettings =
      RideV6.blockchainSettings.copy(functionalitySettings =
        RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = None)
      )
    )
    val settingsWithOnlyXtnBuybackAddress = RideV6.copy(blockchainSettings =
      RideV6.blockchainSettings.copy(functionalitySettings =
        RideV6.blockchainSettings.functionalitySettings.copy(xtnBuybackAddress = Some(xtnBuybackAddress.toString), daoAddress = None)
      )
    )
    val settingsWithBothAddresses = RideV6.copy(blockchainSettings =
      RideV6.blockchainSettings.copy(functionalitySettings =
        RideV6.blockchainSettings.functionalitySettings
          .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString))
      )
    )
    val settingsWithEqualAddresses = RideV6.copy(blockchainSettings =
      RideV6.blockchainSettings.copy(functionalitySettings =
        RideV6.blockchainSettings.functionalitySettings.copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(daoAddress.toString))
      )
    )

    // BlockRewardDistribution is activated, BlockReward is not
    withDomain(settingsWithBothAddresses.setFeaturesHeight(BlockRewardDistribution -> 2, BlockReward -> Int.MaxValue)) { d =>
      d.appendBlock()
      val miner = d.appendBlock().sender.toAddress

      d.balance(daoAddress) shouldBe 0L
      d.balance(xtnBuybackAddress) shouldBe 0L
      d.balance(miner) shouldBe 0L
    }

    // both daoAddress and xtnBuybackAddress are not defined
    withDomain(settingsWithoutAddresses.setFeaturesHeight(BlockRewardDistribution -> 2)) { d =>
      val firstBlock       = d.appendBlock()
      val prevMinerBalance = d.balance(firstBlock.sender.toAddress)
      val miner            = d.appendBlock().sender.toAddress

      d.balance(daoAddress) shouldBe 0L
      d.balance(xtnBuybackAddress) shouldBe 0L
      d.balance(miner) - prevMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial
    }

    // only daoAddress is defined
    withDomain(settingsWithOnlyDaoAddress.setFeaturesHeight(BlockRewardDistribution -> 3)) { d =>
      val firstBlock                   = d.appendBlock()
      val prevMinerBalance             = d.balance(firstBlock.sender.toAddress)
      val miner                        = d.appendBlock().sender.toAddress
      val beforeActivationMinerBalance = d.balance(miner)

      d.balance(daoAddress) shouldBe 0L
      d.balance(xtnBuybackAddress) shouldBe 0L
      beforeActivationMinerBalance - prevMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial

      d.appendBlock()

      val daoAddressBalance = d.balance(daoAddress)
      daoAddressBalance shouldBe d.blockchain.settings.rewardsSettings.initial / 3
      d.balance(xtnBuybackAddress) shouldBe 0L
      d.balance(miner) - beforeActivationMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial - daoAddressBalance
    }

    // only xtnBuybackAddress is defined
    withDomain(settingsWithOnlyXtnBuybackAddress.setFeaturesHeight(BlockRewardDistribution -> 3)) { d =>
      val firstBlock                   = d.appendBlock()
      val prevMinerBalance             = d.balance(firstBlock.sender.toAddress)
      val miner                        = d.appendBlock().sender.toAddress
      val beforeActivationMinerBalance = d.balance(miner)

      d.balance(daoAddress) shouldBe 0L
      d.balance(xtnBuybackAddress) shouldBe 0L
      beforeActivationMinerBalance - prevMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial

      d.appendBlock()

      val xtnBuybackAddressBalance = d.balance(xtnBuybackAddress)
      xtnBuybackAddressBalance shouldBe d.blockchain.settings.rewardsSettings.initial / 3
      d.balance(daoAddress) shouldBe 0L
      d.balance(miner) - beforeActivationMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial - xtnBuybackAddressBalance
    }

    // both daoAddress and xtnBuybackAddress are defined
    withDomain(settingsWithBothAddresses.setFeaturesHeight(BlockRewardDistribution -> 3)) { d =>
      val firstBlock                   = d.appendBlock()
      val prevMinerBalance             = d.balance(firstBlock.sender.toAddress)
      val miner                        = d.appendBlock().sender.toAddress
      val beforeActivationMinerBalance = d.balance(miner)

      d.balance(daoAddress) shouldBe 0L
      d.balance(xtnBuybackAddress) shouldBe 0L
      beforeActivationMinerBalance - prevMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial

      d.appendBlock()

      val daoAddressBalance        = d.balance(daoAddress)
      val xtnBuybackAddressBalance = d.balance(xtnBuybackAddress)
      daoAddressBalance shouldBe d.blockchain.settings.rewardsSettings.initial / 3
      xtnBuybackAddressBalance shouldBe d.blockchain.settings.rewardsSettings.initial / 3
      d.balance(
        miner
      ) - beforeActivationMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial - daoAddressBalance - xtnBuybackAddressBalance
    }

    // both daoAddress and xtnBuybackAddress are defined and equal
    withDomain(settingsWithEqualAddresses.setFeaturesHeight(BlockRewardDistribution -> 3)) { d =>
      val firstBlock                   = d.appendBlock()
      val prevMinerBalance             = d.balance(firstBlock.sender.toAddress)
      val miner                        = d.appendBlock().sender.toAddress
      val beforeActivationMinerBalance = d.balance(miner)

      d.balance(daoAddress) shouldBe 0L
      beforeActivationMinerBalance - prevMinerBalance shouldBe d.blockchain.settings.rewardsSettings.initial

      d.appendBlock()

      val daoAddressBalance = d.balance(daoAddress)
      daoAddressBalance shouldBe 2 * (d.blockchain.settings.rewardsSettings.initial / 3)
      d.balance(
        miner
      ) - beforeActivationMinerBalance shouldBe (d.blockchain.settings.rewardsSettings.initial - daoAddressBalance)
    }
  }

  "Rewards for miner, daoAddress and xtnBuybackAddress should be changed after voting" in {
    val daoAddress        = TxHelpers.address(100)
    val xtnBuybackAddress = TxHelpers.address(101)

    val votingInterval = 10
    val term           = 10

    val settings = BlockRewardDistributionSettings
      .copy(blockchainSettings =
        BlockRewardDistributionSettings.blockchainSettings.copy(
          functionalitySettings = BlockRewardDistributionSettings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = BlockRewardDistributionSettings.blockchainSettings.rewardsSettings.copy(votingInterval = votingInterval, term = term)
        )
      )
      .setFeaturesHeight(BlockReward -> 1)

    withDomain(settings) { d =>
      val initReward              = d.settings.blockchainSettings.rewardsSettings.initial
      val rewardDelta             = d.settings.blockchainSettings.rewardsSettings.minIncrement
      val initialConfigAddrReward = initReward / 3
      val miner                   = d.appendBlock().sender.toAddress
      (1 until votingInterval).foreach { _ =>
        val prevMinerBalance      = d.balance(miner)
        val prevDaoBalance        = d.balance(daoAddress)
        val prevXtnBuybackBalance = d.balance(xtnBuybackAddress)
        d.appendBlock(d.createBlock(Block.ProtoBlockVersion, Seq.empty, rewardVote = initReward - 1))

        d.balance(miner) shouldBe prevMinerBalance + initReward - 2 * initialConfigAddrReward
        d.balance(daoAddress) shouldBe prevDaoBalance + initialConfigAddrReward
        d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackBalance + initialConfigAddrReward
      }

      val prevMinerBalance      = d.balance(miner)
      val prevDaoBalance        = d.balance(daoAddress)
      val prevXtnBuybackBalance = d.balance(xtnBuybackAddress)

      val newReward           = initReward - rewardDelta
      val newConfigAddrReward = newReward / 3

      d.appendBlock()

      d.balance(miner) shouldBe prevMinerBalance + newReward - 2 * newConfigAddrReward
      d.balance(daoAddress) shouldBe prevDaoBalance + newConfigAddrReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackBalance + newConfigAddrReward
    }
  }

  s"NODE-815. XTN buyback and dao addresses should get 2 WAVES when full block reward >= 6 WAVES after ${BlockchainFeatures.CappedReward} activation" in {
    Seq(6.waves, 7.waves).foreach { fullBlockReward =>
      cappedRewardFeatureTestCase(
        fullBlockReward,
        Some(_ => BlockRewardCalculator.MaxAddressReward),
        Some(_ => BlockRewardCalculator.MaxAddressReward)
      )
    }
  }

  s"NODE-816. XTN buyback and dao addresses should get max((R - 2)/2, 0) WAVES when full block reward < 6 WAVES after ${BlockchainFeatures.CappedReward} activation" in {
    Seq(1.waves, 2.waves, 3.waves).foreach { fullBlockReward =>
      cappedRewardFeatureTestCase(
        fullBlockReward,
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0)),
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0))
      )
    }
  }

  s"NODE-817. Single XTN buyback or dao address should get 2 WAVES when full block reward >= 6 WAVES after ${BlockchainFeatures.CappedReward} activation" in {
    Seq(6.waves, 7.waves).foreach { fullBlockReward =>
      // only daoAddress defined
      cappedRewardFeatureTestCase(
        fullBlockReward,
        Some(_ => BlockRewardCalculator.MaxAddressReward),
        None
      )

      // only xtnBuybackAddress defined
      cappedRewardFeatureTestCase(
        fullBlockReward,
        None,
        Some(_ => BlockRewardCalculator.MaxAddressReward)
      )
    }
  }

  s"NODE-818. Single XTN buyback or dao address should get max((R - 2)/2, 0) WAVES when full block reward < 6 WAVES after ${BlockchainFeatures.CappedReward} activation" in {
    Seq(1.waves, 2.waves, 3.waves).foreach { fullBlockReward =>
      // only daoAddress defined
      cappedRewardFeatureTestCase(
        fullBlockReward,
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0)),
        None
      )

      // only xtnBuybackAddress defined
      cappedRewardFeatureTestCase(
        fullBlockReward,
        None,
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0))
      )
    }
  }

  s"NODE-820. Miner should get full block reward when daoAddress and xtnBuybackAddress are not defined after ${BlockchainFeatures.CappedReward} activation" in {
    Seq(1.waves, 2.waves, 3.waves, 6.waves, 7.waves).foreach { fullBlockReward =>
      cappedRewardFeatureTestCase(fullBlockReward, None, None)
    }
  }

  s"NODE-821. Miner should get full block reward after ${BlockchainFeatures.CappedReward} activation if ${BlockchainFeatures.BlockRewardDistribution} is not activated" in {
    Seq(1.waves, 2.waves, 3.waves, 6.waves, 7.waves).foreach { fullBlockReward =>
      // both addresses defined
      cappedRewardFeatureTestCase(fullBlockReward, Some(_ => 0L), Some(_ => 0L), blockRewardDistributionActivated = false)

      // only daoAddress defined
      cappedRewardFeatureTestCase(fullBlockReward, Some(_ => 0L), None, blockRewardDistributionActivated = false)

      // only xtnBuybackAddress defined
      cappedRewardFeatureTestCase(fullBlockReward, None, Some(_ => 0L), blockRewardDistributionActivated = false)

      // both addresses not defined
      cappedRewardFeatureTestCase(fullBlockReward, None, None, blockRewardDistributionActivated = false)
    }
  }

  s"NODE-822. termAfterCappedRewardFeature option should be used instead of term option after ${BlockchainFeatures.CappedReward} activation" in {
    val votingInterval               = 1
    val term                         = 10
    val termAfterCappedRewardFeature = 5

    Seq(5 -> true, 6 -> false, 10 -> true).foreach { case (cappedRewardActivationHeight, rewardChanged) =>
      val settings = BlockRewardDistributionSettings
        .copy(blockchainSettings =
          BlockRewardDistributionSettings.blockchainSettings.copy(
            functionalitySettings = BlockRewardDistributionSettings.blockchainSettings.functionalitySettings
              .copy(daoAddress = None, xtnBuybackAddress = None),
            rewardsSettings = BlockRewardDistributionSettings.blockchainSettings.rewardsSettings
              .copy(votingInterval = votingInterval, term = term, termAfterCappedRewardFeature = termAfterCappedRewardFeature)
          )
        )
        .setFeaturesHeight(BlockchainFeatures.CappedReward -> cappedRewardActivationHeight, BlockchainFeatures.BlockReward -> 1)

      withDomain(settings) { d =>
        val initReward  = d.settings.blockchainSettings.rewardsSettings.initial
        val rewardDelta = d.settings.blockchainSettings.rewardsSettings.minIncrement
        val miner       = d.appendBlock().sender.toAddress
        (1 until cappedRewardActivationHeight - 1).foreach { _ =>
          val prevMinerBalance = d.balance(miner)

          d.appendBlock(d.createBlock(Block.ProtoBlockVersion, Seq.empty, rewardVote = initReward - 1))

          d.balance(miner) shouldBe prevMinerBalance + initReward
        }

        // activation height, if it == last voting interval height then reward for next block will be changed
        d.appendBlock(
          d.createBlock(Block.ProtoBlockVersion, Seq.empty, rewardVote = initReward - 1)
        )

        val prevMinerBalance = d.balance(miner)
        val newReward        = if (rewardChanged) initReward - rewardDelta else initReward

        d.appendBlock()

        d.balance(miner) shouldBe prevMinerBalance + newReward
      }
    }
  }

  s"NODE-825, NODE-828, NODE-841. XTN buyback reward should be cancelled when ${BlockchainFeatures.CeaseXtnBuyback} activated after xtnBuybackRewardPeriod blocks starting from ${BlockchainFeatures.BlockRewardDistribution} activation height (full reward >= 6 WAVES)" in {
    Seq(6.waves, 7.waves).foreach { fullBlockReward =>
      // daoAddress is defined
      ceaseXtnBuybackFeatureTestCase(
        fullBlockReward,
        Some(_ => BlockRewardCalculator.MaxAddressReward),
        Some(_ => BlockRewardCalculator.MaxAddressReward)
      )

      // daoAddress not defined
      ceaseXtnBuybackFeatureTestCase(
        fullBlockReward,
        None,
        Some(_ => BlockRewardCalculator.MaxAddressReward)
      )
    }
  }

  s"NODE-826, NODE-828, NODE-841. XTN buyback reward should be cancelled when ${BlockchainFeatures.CeaseXtnBuyback} activated after xtnBuybackRewardPeriod blocks starting from ${BlockchainFeatures.BlockRewardDistribution} activation height (full reward < 6 WAVES)" in {
    Seq(1.waves, 2.waves, 3.waves).foreach { fullBlockReward =>
      // daoAddress is defined
      ceaseXtnBuybackFeatureTestCase(
        fullBlockReward,
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0)),
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0))
      )

      // daoAddress not defined
      ceaseXtnBuybackFeatureTestCase(
        fullBlockReward,
        None,
        Some(r => Math.max((r - BlockRewardCalculator.GuaranteedMinerReward) / 2, 0))
      )
    }
  }

  s"NODE-829. Miner should get full block reward if daoAddress and xtnBuybackAddress are not defined after ${BlockchainFeatures.CeaseXtnBuyback} activation" in {
    Seq(1.waves, 2.waves, 3.waves, 6.waves, 7.waves).foreach { fullBlockReward =>
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, None, None)
    }
  }

  s"NODE-830. Block reward distribution should not change after ${BlockchainFeatures.CeaseXtnBuyback} activation if ${BlockchainFeatures.CappedReward}/${BlockchainFeatures.CappedReward} and ${BlockchainFeatures.BlockRewardDistribution} not activated" in {
    Seq(1.waves, 2.waves, 3.waves, 6.waves, 7.waves).foreach { fullBlockReward =>
      // both addresses defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, Some(r => r / 3), Some(r => r / 3), cappedRewardActivated = false)

      // only xtnBuybackAddress defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, None, Some(r => r / 3), cappedRewardActivated = false)

      // only daoAddress defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, Some(r => r / 3), None, cappedRewardActivated = false)

      // both addresses not defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, None, None, cappedRewardActivated = false)

      // both addresses defined
      ceaseXtnBuybackFeatureTestCase(
        fullBlockReward,
        Some(_ => 0),
        Some(_ => 0),
        blockRewardDistributionActivated = false,
        cappedRewardActivated = false
      )

      // only xtnBuybackAddress defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, None, Some(_ => 0), blockRewardDistributionActivated = false, cappedRewardActivated = false)

      // only daoAddress defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, Some(_ => 0), None, blockRewardDistributionActivated = false, cappedRewardActivated = false)

      // both addresses not defined
      ceaseXtnBuybackFeatureTestCase(fullBlockReward, None, None, blockRewardDistributionActivated = false, cappedRewardActivated = false)
    }
  }

  s"NODE-858. Rollback on height before ${BlockchainFeatures.BlockRewardDistribution} activation should be correct" in {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)
    val settings          = DomainPresets.ConsensusImprovements
    val rewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 4
      )

    withDomain(rewardSettings) { d =>
      val fullReward = d.blockchain.settings.rewardsSettings.initial

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock() // rollback height
      val prevDaoAddressBalance = d.balance(daoAddress)
      val prevXtnBuybackAddress = d.balance(xtnBuybackAddress)
      val prevMinerBalance      = d.balance(miner)

      prevDaoAddressBalance shouldBe 0
      prevXtnBuybackAddress shouldBe 0
      prevMinerBalance shouldBe fullReward

      d.appendBlock()
      d.appendBlock() // block reward distribution activation height
      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + 2 * (fullReward / 3)
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + 2 * (fullReward / 3)
      d.balance(miner) shouldBe prevMinerBalance + fullReward + 2 * (fullReward - 2 * (fullReward / 3))

      d.appendBlock()
      d.rollbackTo(2)

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance

      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance + fullReward

      d.appendBlock() // block reward distribution activation height

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + fullReward / 3
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + fullReward / 3
      d.balance(miner) shouldBe prevMinerBalance + fullReward + fullReward - 2 * (fullReward / 3)
    }
  }

  s"NODE-859. Rollback on height after ${BlockchainFeatures.BlockRewardDistribution} activation should be correct" in {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)
    val settings          = DomainPresets.ConsensusImprovements
    val rewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 2
      )

    withDomain(rewardSettings) { d =>
      val fullReward = d.blockchain.settings.rewardsSettings.initial

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock() // block reward distribution activation height
      d.appendBlock() // rollback height

      val prevDaoAddressBalance = d.balance(daoAddress)
      val prevXtnBuybackAddress = d.balance(xtnBuybackAddress)
      val prevMinerBalance      = d.balance(miner)

      prevDaoAddressBalance shouldBe 2 * fullReward / 3
      prevXtnBuybackAddress shouldBe 2 * fullReward / 3
      prevMinerBalance shouldBe 2 * (fullReward - (2 * fullReward / 3))

      d.appendBlock()
      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + 2 * (fullReward / 3)
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + 2 * (fullReward / 3)
      d.balance(miner) shouldBe prevMinerBalance + 2 * (fullReward - 2 * (fullReward / 3))

      d.appendBlock()
      d.rollbackTo(3)

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance

      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + fullReward / 3
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + fullReward / 3
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * (fullReward / 3)
    }
  }

  s"NODE-860. Rollback on height before ${BlockchainFeatures.CappedReward} activation should be correct" in {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)
    val settings          = DomainPresets.ConsensusImprovements
    val rewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 2,
        BlockchainFeatures.CappedReward            -> 5
      )

    withDomain(rewardSettings) { d =>
      val fullReward = d.blockchain.settings.rewardsSettings.initial

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock() // block reward distribution activation height
      d.appendBlock() // rollback height

      val prevDaoAddressBalance = d.balance(daoAddress)
      val prevXtnBuybackAddress = d.balance(xtnBuybackAddress)
      val prevMinerBalance      = d.balance(miner)

      prevDaoAddressBalance shouldBe 2 * fullReward / 3
      prevXtnBuybackAddress shouldBe 2 * fullReward / 3
      prevMinerBalance shouldBe 2 * (fullReward - (2 * fullReward / 3))

      d.appendBlock()
      d.appendBlock() // capped reward activation height
      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + fullReward / 3 + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + fullReward / 3 + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * (fullReward / 3) + 2 * (fullReward - 2 * BlockRewardCalculator.MaxAddressReward)

      d.appendBlock()
      d.rollbackTo(3)

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance

      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + fullReward / 3
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + fullReward / 3
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * (fullReward / 3)

      d.appendBlock() // capped reward activation height

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + fullReward / 3 + BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + fullReward / 3 + BlockRewardCalculator.MaxAddressReward
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * (fullReward / 3) + fullReward - 2 * BlockRewardCalculator.MaxAddressReward
    }
  }

  s"NODE-861. Rollback on height after ${BlockchainFeatures.CappedReward} activation should be correct" in {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)
    val settings          = DomainPresets.ConsensusImprovements
    val rewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString)),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 2,
        BlockchainFeatures.CappedReward            -> 2
      )

    withDomain(rewardSettings) { d =>
      val fullReward = d.blockchain.settings.rewardsSettings.initial

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock() // block reward distribution and capped reward activation height
      d.appendBlock() // rollback height

      val prevDaoAddressBalance = d.balance(daoAddress)
      val prevXtnBuybackAddress = d.balance(xtnBuybackAddress)
      val prevMinerBalance      = d.balance(miner)

      prevDaoAddressBalance shouldBe 2 * BlockRewardCalculator.MaxAddressReward
      prevXtnBuybackAddress shouldBe 2 * BlockRewardCalculator.MaxAddressReward
      prevMinerBalance shouldBe 2 * (fullReward - 2 * BlockRewardCalculator.MaxAddressReward)

      d.appendBlock()
      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(miner) shouldBe prevMinerBalance + 2 * (fullReward - 2 * BlockRewardCalculator.MaxAddressReward)

      d.appendBlock()
      d.rollbackTo(3)

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance

      d.appendBlock()

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + BlockRewardCalculator.MaxAddressReward
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * BlockRewardCalculator.MaxAddressReward
    }
  }

  s"NODE-862. Rollback on height before ${BlockchainFeatures.CeaseXtnBuyback} activation should be correct" in {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)
    val settings          = DomainPresets.ConsensusImprovements
    val rewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString), xtnBuybackRewardPeriod = 3),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 2,
        BlockchainFeatures.CappedReward            -> 2,
        BlockchainFeatures.CeaseXtnBuyback         -> 5
      )

    withDomain(rewardSettings) { d =>
      val fullReward = d.blockchain.settings.rewardsSettings.initial

      val miner = d.appendBlock().sender.toAddress
      d.appendBlock() // block reward distribution and capped reward activation height
      d.appendBlock() // rollback height

      val prevDaoAddressBalance = d.balance(daoAddress)
      val prevXtnBuybackAddress = d.balance(xtnBuybackAddress)
      val prevMinerBalance      = d.balance(miner)

      prevDaoAddressBalance shouldBe 2 * BlockRewardCalculator.MaxAddressReward
      prevXtnBuybackAddress shouldBe 2 * BlockRewardCalculator.MaxAddressReward
      prevMinerBalance shouldBe 2 * (fullReward - 2 * BlockRewardCalculator.MaxAddressReward)

      d.appendBlock() // last block of XTN buyback reward period
      d.appendBlock() // cease XTN buyback activation height

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + BlockRewardCalculator.MaxAddressReward
      d.balance(
        miner
      ) shouldBe prevMinerBalance + fullReward - 2 * BlockRewardCalculator.MaxAddressReward + fullReward - BlockRewardCalculator.MaxAddressReward

      d.appendBlock()
      d.rollbackTo(3)

      d.balance(daoAddress) shouldBe prevDaoAddressBalance
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress
      d.balance(miner) shouldBe prevMinerBalance

      d.appendBlock() // last block of XTN buyback reward period

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + BlockRewardCalculator.MaxAddressReward
      d.balance(miner) shouldBe prevMinerBalance + fullReward - 2 * BlockRewardCalculator.MaxAddressReward

      d.appendBlock() // cease XTN buyback activation height

      d.balance(daoAddress) shouldBe prevDaoAddressBalance + 2 * BlockRewardCalculator.MaxAddressReward
      d.balance(xtnBuybackAddress) shouldBe prevXtnBuybackAddress + BlockRewardCalculator.MaxAddressReward
      d.balance(
        miner
      ) shouldBe prevMinerBalance + fullReward - 2 * BlockRewardCalculator.MaxAddressReward + fullReward - BlockRewardCalculator.MaxAddressReward
    }
  }

  private def ceaseXtnBuybackFeatureTestCase(
      fullBlockReward: Long,
      daoAddressRewardF: Option[Long => Long],
      xtnBuybackAddressRewardF: Option[Long => Long],
      blockRewardDistributionActivated: Boolean = true,
      cappedRewardActivated: Boolean = true
  ): Unit = {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)

    val settings                                = DomainPresets.ConsensusImprovements
    val maybeBlockRewardDistributionHeight      = 1
    val blockRewardDistributionActivationHeight = if (blockRewardDistributionActivated) maybeBlockRewardDistributionHeight else Int.MaxValue - 100
    val cappedRewardActivationHeight            = if (cappedRewardActivated) maybeBlockRewardDistributionHeight else Int.MaxValue - 100
    val xtnBuybackRewardPeriod                  = 5

    // feature activation before, at and after last block with XTN buyback address reward
    (xtnBuybackRewardPeriod - maybeBlockRewardDistributionHeight - 1 to xtnBuybackRewardPeriod - maybeBlockRewardDistributionHeight + 3)
      .foreach { ceaseXtnBuybackActivationHeight =>
        val modifiedRewardSettings = settings
          .copy(blockchainSettings =
            settings.blockchainSettings.copy(
              rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = fullBlockReward),
              functionalitySettings = settings.blockchainSettings.functionalitySettings
                .copy(
                  daoAddress = Some(daoAddress.toString).filter(_ => daoAddressRewardF.isDefined),
                  xtnBuybackAddress = Some(xtnBuybackAddress.toString).filter(_ => xtnBuybackAddressRewardF.isDefined),
                  xtnBuybackRewardPeriod = xtnBuybackRewardPeriod
                )
            )
          )
          .setFeaturesHeight(
            BlockchainFeatures.BlockRewardDistribution -> blockRewardDistributionActivationHeight,
            BlockchainFeatures.CappedReward            -> cappedRewardActivationHeight,
            BlockchainFeatures.CeaseXtnBuyback         -> ceaseXtnBuybackActivationHeight
          )

        withDomain(modifiedRewardSettings) { d =>
          val firstBlock = d.appendBlock()
          val miner      = firstBlock.sender.toAddress

          (1 to maybeBlockRewardDistributionHeight + xtnBuybackRewardPeriod + 1).foreach { idx =>
            val prevMinerBalance             = d.balance(miner)
            val prevDaoAddressBalance        = d.balance(daoAddress)
            val prevXtnBuybackAddressBalance = d.balance(xtnBuybackAddress)

            d.appendBlock()

            val daoAddressReward        = d.balance(daoAddress) - prevDaoAddressBalance
            val xtnBuybackAddressReward = d.balance(xtnBuybackAddress) - prevXtnBuybackAddressBalance

            daoAddressReward shouldBe daoAddressRewardF.map(_.apply(fullBlockReward)).getOrElse(0L)
            val expectedXtnBuybackAddressReward =
              if (ceaseXtnBuybackActivationHeight <= idx + 1 && blockRewardDistributionActivationHeight + xtnBuybackRewardPeriod <= idx + 1) 0
              else xtnBuybackAddressRewardF.map(_.apply(fullBlockReward)).getOrElse(0L)
            xtnBuybackAddressReward shouldBe expectedXtnBuybackAddressReward
            d.balance(
              miner
            ) - prevMinerBalance shouldBe d.settings.blockchainSettings.rewardsSettings.initial - daoAddressReward - xtnBuybackAddressReward
          }
        }
      }
  }

  private def cappedRewardFeatureTestCase(
      fullBlockReward: Long,
      daoAddressRewardF: Option[Long => Long],
      xtnBuybackAddressRewardF: Option[Long => Long],
      blockRewardDistributionActivated: Boolean = true
  ) = {
    val daoAddress        = TxHelpers.address(1)
    val xtnBuybackAddress = TxHelpers.address(2)

    val settings                      = DomainPresets.ConsensusImprovements
    val blockRewardDistributionHeight = if (blockRewardDistributionActivated) 0 else Int.MaxValue
    val modifiedRewardSettings = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = fullBlockReward),
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(
              daoAddress = Some(daoAddress.toString).filter(_ => daoAddressRewardF.isDefined),
              xtnBuybackAddress = Some(xtnBuybackAddress.toString).filter(_ => xtnBuybackAddressRewardF.isDefined)
            )
        )
      )
      .setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> blockRewardDistributionHeight, BlockchainFeatures.CappedReward -> 3)

    withDomain(modifiedRewardSettings) { d =>
      val firstBlock = d.appendBlock()
      val miner      = firstBlock.sender.toAddress

      d.balance(daoAddress) shouldBe 0
      d.balance(xtnBuybackAddress) shouldBe 0
      d.balance(miner) shouldBe 0

      d.appendBlock()

      val prevDaoAddressBalance        = d.balance(daoAddress)
      val prevXtnBuybackAddressBalance = d.balance(xtnBuybackAddress)
      val prevMinerBalance             = d.balance(miner)

      prevDaoAddressBalance shouldBe (if (daoAddressRewardF.isDefined && blockRewardDistributionActivated) fullBlockReward / 3 else 0L)
      prevXtnBuybackAddressBalance shouldBe (if (xtnBuybackAddressRewardF.isDefined && blockRewardDistributionActivated) fullBlockReward / 3 else 0L)
      prevMinerBalance shouldBe fullBlockReward - prevDaoAddressBalance - prevXtnBuybackAddressBalance

      d.appendBlock()

      val daoAddressReward        = d.balance(daoAddress) - prevDaoAddressBalance
      val xtnBuybackAddressReward = d.balance(xtnBuybackAddress) - prevXtnBuybackAddressBalance
      val minerReward             = d.balance(miner) - prevMinerBalance

      daoAddressReward shouldBe daoAddressRewardF.map(_.apply(fullBlockReward)).getOrElse(0L)
      xtnBuybackAddressReward shouldBe xtnBuybackAddressRewardF.map(_.apply(fullBlockReward)).getOrElse(0L)
      minerReward shouldBe fullBlockReward - daoAddressReward - xtnBuybackAddressReward
    }
  }

  private val daoAddress        = TxHelpers.address(10002)
  private val xtnBuybackAddress = TxHelpers.address(10003)
  private val settingsWithRewardBoost = DomainPresets.BlockRewardDistribution
    .setFeaturesHeight(BlockchainFeatures.BoostBlockReward -> 5)
    .configure(fs =>
      fs.copy(
        blockRewardBoostPeriod = 10,
        daoAddress = Some(daoAddress.toString),
        xtnBuybackAddress = Some(xtnBuybackAddress.toString)
      )
    )

  private val blockMiner          = TxHelpers.signer(10001)
  private val initialMinerBalance = 100_000.waves
  private val boostedShare = 2.waves * 10
  private val boostedShareAfterIncrease = 6.5.waves / 3 * 10
  private val boostedMinerShareAfterIncrease: Long = 65.waves - 2 * boostedShareAfterIncrease
  private val shareAfterIncrease = 6.5.waves / 3
  private val minerShareAfterIncrease = 6.5.waves - 2 * shareAfterIncrease

  private def assertBalances(blockchain: Blockchain, expectedBalances: (Address, Long)*)(implicit pos: Position): Unit =
    expectedBalances.foreach { case (address, balance) =>
      withClue(address) {
        blockchain.balance(address) shouldEqual balance
      }
    }

  "Boost block reward feature activation" in withDomain(
    settingsWithRewardBoost.copy(blockchainSettings =
      settingsWithRewardBoost.blockchainSettings.copy(
        rewardsSettings = RewardsSettings(10, 10, 6.waves, 0.5.waves, 4)
      )
    ),
    Seq(AddrWithBalance(blockMiner.toAddress, initialMinerBalance))
  ) { d =>
    (1 to 3).foreach(_ => d.appendKeyBlock(blockMiner))
    // height 4: before activation
    d.blockchain.height shouldBe 4
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + 2.waves * 3),
      daoAddress           -> 2.waves * 3,
      xtnBuybackAddress    -> 2.waves * 3
    )

    d.appendKeyBlock(blockMiner)
    // height 5: activation height
    val rewardAtActivationHeight = 2.waves * 3 + boostedShare
    d.blockchain.height shouldBe 5
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + rewardAtActivationHeight),
      daoAddress           -> rewardAtActivationHeight,
      xtnBuybackAddress    -> rewardAtActivationHeight
    )

    d.appendKeyBlock(blockMiner)
    // height 7: start voting
    (1 to 3).foreach(_ => d.appendBlock(d.createBlock(Block.RewardBlockVersion, Seq.empty, generator = blockMiner, rewardVote = 7.waves)))
    d.blockchain.height shouldBe 9
    val rewardBeforeIncrease = rewardAtActivationHeight + 4 * boostedShare
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + rewardBeforeIncrease),
      daoAddress           -> rewardBeforeIncrease,
      xtnBuybackAddress    -> rewardBeforeIncrease
    )

    // height 10: new base reward value = 65 waves
    d.appendBlock(d.createBlock(Block.RewardBlockVersion, Seq.empty, generator = blockMiner, rewardVote = 7.waves))
    d.blockchain.height shouldBe 10
    val rewardAfterIncrease = rewardBeforeIncrease + boostedShareAfterIncrease
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + rewardBeforeIncrease + boostedMinerShareAfterIncrease),
      daoAddress           -> rewardAfterIncrease,
      xtnBuybackAddress    -> rewardAfterIncrease
    )

    (1 to 4).foreach(_ => d.appendKeyBlock(blockMiner))
    // height 14: before deactivation
    d.blockchain.height shouldBe 14
    val rewardBeforeDeactivation = rewardAfterIncrease + 4 * boostedShareAfterIncrease
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + rewardBeforeIncrease + 5 * boostedMinerShareAfterIncrease),
      daoAddress           -> rewardBeforeDeactivation,
      xtnBuybackAddress    -> rewardBeforeDeactivation
    )

    d.appendKeyBlock(blockMiner)
    // height 15: deactivation
    d.blockchain.height shouldBe 15
    val rewardAfterDeactivation = rewardBeforeDeactivation + shareAfterIncrease
    assertBalances(
      d.blockchain,
      blockMiner.toAddress -> (initialMinerBalance + rewardBeforeIncrease + 5 * boostedMinerShareAfterIncrease + minerShareAfterIncrease),
      daoAddress           -> rewardAfterDeactivation,
      xtnBuybackAddress    -> rewardAfterDeactivation
    )
  }
}
