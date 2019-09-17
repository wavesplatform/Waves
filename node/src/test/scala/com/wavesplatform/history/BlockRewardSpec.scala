package com.wavesplatform.history

import cats.syntax.option._
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, RewardsSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockRewardSpec extends FreeSpec with ScalaCheckPropertyChecks with WithDomain with Matchers with TransactionGen with NoShrink {

  private val BlockRewardActivationHeight = 5
  private val NGActivationHeight          = 0
  private val InitialReward               = 6 * Constants.UnitsInWave
  private val rewardSettings = settings.copy(
    blockchainSettings = DefaultBlockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockReward.id    -> BlockRewardActivationHeight,
          BlockchainFeatures.NG.id             -> NGActivationHeight,
          BlockchainFeatures.FeeSponsorship.id -> -10
        )
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
        GenesisTransaction.create(sourceAddress, (Constants.TotalWaves - 60000) * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(issuer, 40000 * Constants.UnitsInWave, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner1, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(miner2, InitialMinerBalance, ntpTime.getTimestamp()).explicitGet()
      )
    )

  } yield (sourceAddress, issuer, miner1, miner2, genesisBlock)

  private val activationScenario = for {
    (sourceAddress, _, miner, _, genesisBlock) <- genesis
    recipient                                  <- accountGen
    transfers                                  <- Gen.listOfN(10, transferGeneratorP(ntpNow, sourceAddress, recipient, 1000 * Constants.UnitsInWave))
    b2              = TestBlock.create(ntpNow, genesisBlock.uniqueId, transfers, miner)
    b3              = mkEmptyBlock(b2.uniqueId, miner)
    b4              = mkEmptyBlock(b3.uniqueId, miner)
    b5              = mkEmptyBlock(b4.uniqueId, miner)
    b6              = mkEmptyBlock(b5.uniqueId, miner)
    b7              = mkEmptyBlock(b6.uniqueId, miner)
    b8              = mkEmptyBlock(b7.uniqueId, miner)
    b9              = mkEmptyBlock(b8.uniqueId, miner)
    b10             = mkEmptyBlock(b9.uniqueId, miner)
    b11             = mkEmptyBlockIncReward(b10.uniqueId, miner)
    b12             = mkEmptyBlockIncReward(b11.uniqueId, miner)
    b13             = mkEmptyBlockIncReward(b12.uniqueId, miner)
    b14             = mkEmptyBlockIncReward(b13.uniqueId, miner)
    b15             = mkEmptyBlockIncReward(b14.uniqueId, miner)
    secondTermStart = BlockRewardActivationHeight + 10
    b16 = Range
      .inclusive(secondTermStart + 1, secondTermStart + rewardSettings.blockchainSettings.rewardsSettings.term)
      .foldLeft(Seq(b15)) {
        case (prev, i) if rewardSettings.blockchainSettings.rewardsSettings.votingWindow(BlockRewardActivationHeight, i).contains(i) =>
          prev :+ mkEmptyBlockDecReward(prev.last.uniqueId, miner)
        case (prev, _) => prev :+ mkEmptyBlock(prev.last.uniqueId, miner)
      }
      .tail
  } yield (miner, transfers, Seq(genesisBlock, b2), Seq(b3, b4), b5, Seq(b6, b7, b8, b9), Seq(b10, b11, b12, b13, b14), b15, b16)

  "Miner receives reward as soon as the feature is activated and changes reward amount after voting" in forAll(activationScenario) {
    case (miner, transfers, b1s, b2s, activationBlock, b3s, b4s, newTermBlock, b5s) =>
      withDomain(rewardSettings) { d =>
        val totalFee = transfers.map(_.fee).sum

        b1s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())

        b2s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe false
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight - 1) shouldBe None
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(activationBlock).explicitGet()
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight) shouldBe Some(InitialReward)
        d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialReward + InitialMinerBalance + totalFee

        b3s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 4
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 4) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 5 * InitialReward + InitialMinerBalance + totalFee

        b4s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 9
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 9) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + InitialMinerBalance + totalFee

        val NextReward = InitialReward + 1 * Constants.UnitsInWave

        d.blockchainUpdater.processBlock(newTermBlock).explicitGet()
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + NextReward + InitialMinerBalance + totalFee
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10) shouldBe NextReward.some

        b5s.init.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10 - 1
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10 - 1) shouldBe NextReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + 10 * NextReward + InitialMinerBalance + totalFee

        d.blockchainUpdater.processBlock(b5s.last).explicitGet()

        d.blockchainUpdater.height shouldEqual BlockRewardActivationHeight + 10 + 10
        d.blockchainUpdater.isFeatureActivated(BlockchainFeatures.BlockReward) shouldBe true
        d.blockchainUpdater.blockReward(BlockRewardActivationHeight + 10 + 10) shouldBe InitialReward.some
        d.blockchainUpdater.balance(miner.toAddress) shouldBe 10 * InitialReward + 10 * NextReward + InitialReward + InitialMinerBalance + totalFee
      }
  }

  "Miner receives reward and fees" - {
    val ngEmptyScenario = for {
      (sourceAddress, issuer, miner1, miner2, genesisBlock) <- genesis
      tx1 = TransferTransactionV1
        .selfSigned(Waves, issuer, sourceAddress, 10 * Constants.UnitsInWave, ntpTime.getTimestamp(), Waves, OneTotalFee, Array.emptyByteArray)
        .explicitGet()
      tx2 = TransferTransactionV1
        .selfSigned(Waves, issuer, sourceAddress, 10 * Constants.UnitsInWave, ntpTime.getTimestamp(), Waves, OneTotalFee, Array.emptyByteArray)
        .explicitGet()
      b2        = mkEmptyBlock(genesisBlock.uniqueId, miner1)
      b3        = mkEmptyBlock(b2.uniqueId, miner1)
      b4        = TestBlock.create(ntpNow, b3.uniqueId, Seq(tx1), miner1)
      (b5, m5s) = chainBaseAndMicro(b4.uniqueId, Seq.empty, Seq(Seq(tx2)), miner2, 3, ntpNow)
    } yield (miner1, miner2, Seq(genesisBlock, b2, b3, b4), b5, m5s)

    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): BlockDiffer.Result =
      BlockDiffer.fromBlock(blockchain, prevBlock, b, MiningConstraint.Unlimited: MiningConstraint).explicitGet()

    "when NG state is empty" in forAll(ngEmptyScenario) {
      case (miner1, miner2, b2s, b3, m3s) =>
        withDomain(rewardSettings) { d =>
          b2s.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
            val BlockDiffer.Result(diff, carryFee, totalFee, _, _) = differ(d.levelDBWriter, prevBlock, curBlock)
            d.levelDBWriter.append(diff, carryFee, totalFee, None, curBlock)
            Some(curBlock)
          }

          d.levelDBWriter.height shouldBe BlockRewardActivationHeight - 1
          d.levelDBWriter.balance(miner1.toAddress) shouldBe InitialMinerBalance + OneFee
          d.levelDBWriter.totalFee(BlockRewardActivationHeight - 1) shouldBe OneTotalFee.some
          d.levelDBWriter.carryFee shouldBe OneCarryFee

          d.blockchainUpdater.processBlock(b3).explicitGet()
          d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneCarryFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight) shouldBe 0L.some
          d.blockchainUpdater.carryFee shouldBe 0L

          m3s.foreach(mb => d.blockchainUpdater.processMicroBlock(mb).explicitGet())

          d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
          d.blockchainUpdater.balance(miner2.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + OneCarryFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight) shouldBe OneTotalFee.some
          d.blockchainUpdater.carryFee shouldBe OneCarryFee
        }
    }

    val betterBlockScenario = for {
      (sourceAddress, issuer, miner, _, genesisBlock) <- genesis
      tx = TransferTransactionV1
        .selfSigned(Waves, issuer, sourceAddress, 10 * Constants.UnitsInWave, ntpTime.getTimestamp(), Waves, OneTotalFee, Array.emptyByteArray)
        .explicitGet()
      b2        = mkEmptyBlock(genesisBlock.uniqueId, miner)
      b3        = mkEmptyBlock(b2.uniqueId, miner)
      b4        = mkEmptyBlock(b3.uniqueId, miner)
      (b5, m5s) = chainBaseAndMicro(b4.uniqueId, Seq.empty, Seq(Seq(tx)), miner, 3, ntpNow)
      b6a       = TestBlock.create(ntpNow, m5s.last.totalResBlockSig, Seq.empty, miner)
      b6b       = TestBlock.sign(miner, b6a.copy(consensusData = b6a.consensusData.copy(baseTarget = b6a.consensusData.baseTarget - 1L)))
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5), m5s, b6a, b6b)

    "when received better liquid block" in forAll(betterBlockScenario) {
      case (miner, b1s, m1s, b2a, b2b) =>
        withDomain(rewardSettings) { d =>
          b1s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
          m1s.foreach(m => d.blockchainUpdater.processMicroBlock(m).explicitGet())

          d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
          d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight) shouldBe OneTotalFee.some
          d.blockchainUpdater.carryFee shouldBe OneCarryFee

          d.blockchainUpdater.processBlock(b2a).explicitGet()
          d.blockchainUpdater.processBlock(b2b).explicitGet()

          d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + InitialReward + OneCarryFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight + 1) shouldBe 0L.some
          d.blockchainUpdater.carryFee shouldBe 0L
        }
    }

    val sameButBetterBlockScenario = for {
      (sourceAddress, issuer, miner, _, genesisBlock) <- genesis
      tx1 = TransferTransactionV1
        .selfSigned(Waves, issuer, sourceAddress, 10 * Constants.UnitsInWave, ntpTime.getTimestamp(), Waves, OneTotalFee, Array.emptyByteArray)
        .explicitGet()
      tx2 = TransferTransactionV1
        .selfSigned(Waves, issuer, sourceAddress, 10 * Constants.UnitsInWave, ntpTime.getTimestamp(), Waves, OneTotalFee, Array.emptyByteArray)
        .explicitGet()
      b2        = mkEmptyBlock(genesisBlock.uniqueId, miner)
      b3        = mkEmptyBlock(b2.uniqueId, miner)
      b4        = mkEmptyBlock(b3.uniqueId, miner)
      (b5, m5s) = chainBaseAndMicro(b4.uniqueId, Seq.empty, Seq(Seq(tx1)), miner, 3, ntpNow)
      b6a       = TestBlock.create(ntpNow, m5s.last.totalResBlockSig, Seq.empty, miner)
      b6b       = TestBlock.sign(miner, b6a.copy(transactionData = Seq(tx2)))
    } yield (miner, Seq(genesisBlock, b2, b3, b4, b5), m5s, b6a, b6b)

    "when received same liquid block but it is better than existing" in forAll(sameButBetterBlockScenario) {
      case (miner, b1s, m1s, b2a, b2b) =>
        withDomain(rewardSettings) { d =>
          b1s.foreach(b => d.blockchainUpdater.processBlock(b).explicitGet())
          m1s.foreach(m => d.blockchainUpdater.processMicroBlock(m).explicitGet())

          d.blockchainUpdater.height shouldBe BlockRewardActivationHeight
          d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight) shouldBe OneTotalFee.some
          d.blockchainUpdater.carryFee shouldBe OneCarryFee

          d.blockchainUpdater.processBlock(b2a).explicitGet()
          d.blockchainUpdater.processBlock(b2b).explicitGet()

          d.blockchainUpdater.balance(miner.toAddress) shouldBe InitialMinerBalance + InitialReward + OneFee + InitialReward + OneFee + OneCarryFee
          d.blockchainUpdater.totalFee(BlockRewardActivationHeight + 1) shouldBe OneTotalFee.some
          d.blockchainUpdater.carryFee shouldBe OneCarryFee
        }
    }
  }
}
