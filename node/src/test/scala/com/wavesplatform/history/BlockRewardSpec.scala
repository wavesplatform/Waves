package com.wavesplatform.history

import cats.syntax.option.*
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.RewardApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.Keys
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockReward, ConsensusImprovements}
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, RewardsSettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.test.DomainPresets.{RideV6, WavesSettingsOps}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers}
import org.scalacheck.Gen

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
        InitialReward,
        1 * Constants.UnitsInWave,
        4
      )
    )
  )

  private def mkEmptyBlock(ref: ByteStr, signer: KeyPair): Block = TestBlock.create(ntpNow, ref, Seq.empty, signer)

  private def mkEmptyBlockIncReward(ref: ByteStr, signer: KeyPair): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = InitialReward + 1 * Constants.UnitsInWave, version = Block.RewardBlockVersion)

  private def mkEmptyBlockDecReward(ref: ByteStr, signer: KeyPair): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = InitialReward - 1 * Constants.UnitsInWave, version = Block.RewardBlockVersion)

  private def mkEmptyBlockReward(ref: ByteStr, signer: KeyPair, vote: Long): Block =
    TestBlock.create(ntpNow, ref, Seq.empty, signer, rewardVote = vote, version = Block.RewardBlockVersion)

  private val InitialMinerBalance = 10000 * Constants.UnitsInWave
  private val OneTotalFee         = 100000
  private val OneCarryFee         = (OneTotalFee * 0.6).toLong
  private val OneFee              = (OneTotalFee * 0.4).toLong

  private val genesis = for {
    sourceAddress <- accountGen
    issuer        <- accountGen
    miner1        <- accountGen
    miner2        <- accountGen
    genesisBlock = TestBlock.create(
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

  } yield (sourceAddress, issuer, miner1, miner2, genesisBlock)

  private val activationScenario = for {
    (sourceAddress, _, miner, _, genesisBlock) <- genesis
    recipient                                  <- accountGen
    transfers <- Gen.listOfN(10, transferGeneratorP(ntpNow, sourceAddress, recipient.toAddress, 1000 * Constants.UnitsInWave))
    b2              = TestBlock.create(ntpNow, genesisBlock.id(), transfers, miner)
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
        case (prev, i) if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i).contains(i) =>
          prev :+ mkEmptyBlockDecReward(prev.last.id(), miner)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.id(), miner)
      }
      .tail
    thirdTermStart = BlockRewardActivationHeight + 10 + 10
    b17 = Range
      .inclusive(thirdTermStart + 1, thirdTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b16.last)) {
        case (prev, i) if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i).contains(i) =>
          prev :+ mkEmptyBlockReward(prev.last.id(), miner, -1L)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.id(), miner)
      }
      .tail
    fourthTermStart = BlockRewardActivationHeight + 10 + 10 + 10
    b18 = Range
      .inclusive(fourthTermStart + 1, fourthTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b17.last)) {
        case (prev, i) if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i).contains(i) =>
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
      b4        = TestBlock.create(ntpNow, b3.id(), Seq(tx1), miner1)
      (b5, m5s) = chainBaseAndMicro(b4.id(), Seq.empty, Seq(Seq(tx2)), miner2, 3.toByte, ntpNow)
    } yield (miner1, miner2, Seq(genesisBlock, b2, b3, b4), b5, m5s)

    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): BlockDiffer.Result =
      BlockDiffer.fromBlock(blockchain, prevBlock, b, MiningConstraint.Unlimited: MiningConstraint, b.header.generationSignature).explicitGet()

    "when NG state is empty" in forAll(ngEmptyScenario) { case (miner1, miner2, b2s, b3, m3s) =>
      withDomain(rewardSettings) { d =>
        b2s.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
          val BlockDiffer.Result(diff, carryFee, totalFee, _, _) = differ(d.levelDBWriter, prevBlock, curBlock)
          d.levelDBWriter.append(diff, carryFee, totalFee, None, curBlock.header.generationSignature, curBlock)
          Some(curBlock)
        }

        d.levelDBWriter.height shouldBe BlockRewardActivationHeight - 1
        d.levelDBWriter.balance(miner1.toAddress) shouldBe InitialMinerBalance + OneFee
        d.db.get(Keys.blockMetaAt(Height(BlockRewardActivationHeight - 1))).map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.levelDBWriter.carryFee shouldBe OneCarryFee

        d.blockchainUpdater.processBlock(b3) should beRight
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe 0L.some
        d.blockchainUpdater.carryFee shouldBe 0L

        m3s.foreach(mb => d.blockchainUpdater.processMicroBlock(mb) should beRight)

        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee shouldBe OneCarryFee
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
      b6a       = TestBlock.create(ntpNow, m5s.last.totalResBlockSig, Seq.empty, miner)
      b6b = TestBlock.sign(
        miner,
        b6a.copy(header = b6a.header.copy(baseTarget = b6a.header.baseTarget - 1L))
      )
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5), m5s, b6a, b6b)

    "when received better liquid block" in forAll(betterBlockScenario) { case (miner, b1s, m1s, b2a, b2b) =>
      withDomain(rewardSettings) { d =>
        b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        m1s.foreach(m => d.blockchainUpdater.processMicroBlock(m) should beRight)

        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee shouldBe OneCarryFee

        d.blockchainUpdater.processBlock(b2a) should beRight
        d.blockchainUpdater.processBlock(b2b) should beRight

        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + InitialReward + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe 0L.some
        d.blockchainUpdater.carryFee shouldBe 0L
      }
    }

    val sameButBetterBlockScenario = for {
      (sourceAddress, issuer, miner, _, genesisBlock) <- genesis
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
      b2        = mkEmptyBlock(genesisBlock.id(), miner)
      b3        = mkEmptyBlock(b2.id(), miner)
      b4        = mkEmptyBlock(b3.id(), miner)
      (b5, m5s) = chainBaseAndMicro(b4.id(), Seq.empty, Seq(Seq(tx1)), miner, 3.toByte, ntpNow)
      b6a       = TestBlock.create(ntpNow, m5s.last.totalResBlockSig, Seq.empty, miner)
      b6b       = TestBlock.sign(miner, b6a.copy(transactionData = Seq(tx2)))
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5), m5s, b6a, b6b)

    "when received same liquid block but it is better than existing" in forAll(sameButBetterBlockScenario) { case (miner, b1s, m1s, b2a, b2b) =>
      withDomain(rewardSettings) { d =>
        b1s.foreach(b => d.blockchainUpdater.processBlock(b) should beRight)
        m1s.foreach(m => d.blockchainUpdater.processMicroBlock(m) should beRight)

        d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee shouldBe OneCarryFee

        d.blockchainUpdater.processBlock(b2a) should beRight
        d.blockchainUpdater.processBlock(b2b) should beRight

        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + InitialReward + OneFee + OneCarryFee
        d.blockchainUpdater.liquidBlockMeta.map(_.totalFeeInWaves) shouldBe OneTotalFee.some
        d.blockchainUpdater.carryFee shouldBe OneCarryFee
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
          case (prev, i) if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i).contains(i) =>
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
      rewardsSettings = RewardsSettings(12, 6 * Constants.UnitsInWave, 1 * Constants.UnitsInWave, 6)
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
      calcSettings.nearestTermEnd(4, 9) shouldBe 15
      calcSettings.nearestTermEnd(4, 10) shouldBe 15

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
      rewardsSettings = RewardsSettings(3, 6 * Constants.UnitsInWave, 1 * Constants.UnitsInWave, 2)
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
}
