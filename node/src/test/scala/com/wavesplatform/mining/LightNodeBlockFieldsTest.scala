package com.wavesplatform.mining

import com.wavesplatform.account.SeedKeyPair
import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.mining.MultiDimensionalMiningConstraint.Unlimited
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner, transfer}
import com.wavesplatform.transaction.TxValidationError.GenericError
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.concurrent.duration.DurationInt

class LightNodeBlockFieldsTest extends PropSpec with WithDomain {
  property("new block fields appear `lightNodeBlockFieldsAbsenceInterval` blocks after LightNode activation") {
    withDomain(
      TransactionStateSnapshot.setFeaturesHeight(LightNode -> 2).configure(_.copy(lightNodeBlockFieldsAbsenceInterval = 10)),
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
    ) { d =>
      withMiner(
        d.blockchain,
        d.testTime,
        d.settings.copy(minerSettings = d.settings.minerSettings.copy(quorum = 0, minMicroBlockAge = 0.seconds)),
        verify = false,
        timeDrift = Int.MaxValue
      ) { case (miner, append) =>
        val microBlockMiner = new MicroBlockMinerImpl(
          _ => (),
          null,
          d.blockchainUpdater,
          d.utxPool,
          d.settings.minerSettings,
          miner.minerScheduler,
          miner.appenderScheduler,
          Observable.empty,
          identity
        )
        val challenger = new BlockChallengerImpl(
          d.blockchain,
          new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
          d.wallet,
          d.settings,
          d.testTime,
          d.posSelector,
          b => Task.now(append(b)),
          timeDrift = Int.MaxValue
        ) {
          override def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)] = Right((defaultSigner, 0))
        }
        def block(height: Int) = d.blocksApi.blockAtHeight(height).get._1.header
        def appendBlock()      = append(miner.forgeBlock(defaultSigner).explicitGet()._1).explicitGet()
        def appendMicro() = {
          d.utxPool.putIfNew(transfer()).resultE.explicitGet()
          microBlockMiner.generateOneMicroBlockTask(defaultSigner, d.lastBlock, Unlimited, 0).runSyncUnsafe()
        }
        def challengeBlock() = {
          val invalidBlock = d.createBlock(ProtoBlockVersion, Seq(), strictTime = true, stateHash = Some(Some(ByteStr.fill(DigestLength)(1))))
          challenger.challengeBlock(invalidBlock, null).runSyncUnsafe()
        }

        appendBlock()
        d.blockchain.height shouldBe 2
        d.blockchain.isFeatureActivated(LightNode) shouldBe true
        block(2).stateHash shouldBe None

        appendMicro()
        block(2).stateHash shouldBe None

        challengeBlock()
        d.blockchain.height shouldBe 3
        block(3).stateHash shouldBe None
        block(3).challengedHeader shouldBe None

        (1 to 8).foreach(_ => appendBlock())
        d.blockchain.height shouldBe 11
        block(11).stateHash shouldBe None

        appendMicro()
        block(11).stateHash shouldBe None

        appendBlock()
        d.blockchain.height shouldBe 12
        val hash1 = block(12).stateHash
        hash1 shouldBe defined

        appendMicro()
        val hash2 = block(12).stateHash
        hash2 shouldBe defined
        hash2 should not be hash1

        d.rollbackTo(10)
        challengeBlock()
        block(11).stateHash shouldBe None
        block(11).challengedHeader shouldBe None

        challengeBlock()
        block(12).stateHash shouldBe defined
        block(12).challengedHeader shouldBe defined
      }
    }
  }

  property(
    "blocks with challenged header or state hash should be allowed only `lightNodeBlockFieldsAbsenceInterval` blocks after LightNode activation"
  ) {
    withDomain(
      TransactionStateSnapshot.setFeaturesHeight(LightNode -> 2).configure(_.copy(lightNodeBlockFieldsAbsenceInterval = 10)),
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
    ) { d =>
      withMiner(d.blockchain, d.testTime, d.settings, verify = false, timeDrift = Int.MaxValue) { case (_, append) =>
        (1 to 9).foreach(_ => d.appendBlock())
        d.blockchain.height shouldBe 10
        val challengedBlock  = d.createBlock(ProtoBlockVersion, Nil, stateHash = Some(Some(ByteStr.fill(DigestLength)(1))))
        val challengingBlock = d.createChallengingBlock(defaultSigner, challengedBlock)
        append(challengedBlock) should produce("UnexpectedLightNodeFields")
        append(challengingBlock) should produce("UnexpectedLightNodeFields")

        d.appendBlock()
        d.blockchain.height shouldBe 11
        val correctBlockWithStateHash = d.createBlock(ProtoBlockVersion, Nil)
        correctBlockWithStateHash.header.stateHash shouldBe defined
        append(correctBlockWithStateHash) shouldBe a[Right[?, ?]]

        d.rollbackTo(11)
        val invalidBlock      = d.createBlock(ProtoBlockVersion, Nil, stateHash = Some(Some(ByteStr.fill(DigestLength)(1))))
        val challengingBlock2 = d.createChallengingBlock(defaultSigner, invalidBlock)
        append(challengingBlock2) shouldBe a[Right[?, ?]]
      }
    }
  }
}
