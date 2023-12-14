package com.wavesplatform.mining

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
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.concurrent.duration.DurationInt

class LightNodeBlockFieldsTest extends PropSpec with WithDomain {
  private val invalidStateHash = Some(Some(ByteStr.fill(DigestLength)(1)))

  property("new block fields appear `lightNodeBlockFieldsAbsenceInterval` blocks after LightNode activation") {
    withDomain(
      TransactionStateSnapshot.setFeaturesHeight(LightNode -> 2).configure(_.copy(lightNodeBlockFieldsAbsenceInterval = 10)),
      AddrWithBalance.enoughBalances(defaultSigner, secondSigner)
    ) { d =>
      withMiner(
        d.blockchain,
        d.testTime,
        d.settings.copy(minerSettings = d.settings.minerSettings.copy(quorum = 0, minMicroBlockAge = 0.seconds)),
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
        def block(height: Int) = d.blocksApi.blockAtHeight(height).get._1.header
        def appendBlock() = {
          val block = miner.forgeBlock(defaultSigner).explicitGet()._1
          d.testTime.setTime(block.header.timestamp)
          append(block).explicitGet()
        }
        def appendMicro() = {
          d.utxPool.putIfNew(transfer()).resultE.explicitGet()
          microBlockMiner.generateOneMicroBlockTask(defaultSigner, d.lastBlock, Unlimited, 0).runSyncUnsafe()
        }

        appendBlock()
        d.blockchain.height shouldBe 2
        d.blockchain.isFeatureActivated(LightNode) shouldBe true
        block(2).stateHash shouldBe None

        appendMicro()
        block(2).stateHash shouldBe None

        (1 to 9).foreach(_ => appendBlock())
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
      withMiner(d.blockchain, d.testTime, d.settings) { case (_, append) =>
        (1 to 9).foreach(_ => d.appendBlock())
        d.blockchain.height shouldBe 10
        val challengedBlock  = d.createBlock(ProtoBlockVersion, Nil, strictTime = true, stateHash = invalidStateHash)
        val challengingBlock = d.createChallengingBlock(secondSigner, challengedBlock, strictTime = true)
        val blockWithOnlyChallengingHeader = {
          val challengedHeader = challengingBlock.header.challengedHeader.map(_.copy(stateHash = None))
          val block            = d.createBlock(ProtoBlockVersion, Nil, strictTime = true, challengedHeader = challengedHeader)
          block.copy(header = block.header.copy(stateHash = None))
        }
        d.testTime.setTime(challengingBlock.header.timestamp)
        append(challengedBlock) should produce("Block state hash is not supported yet")
        append(challengingBlock) should produce("Block state hash is not supported yet")
        append(blockWithOnlyChallengingHeader) should produce("Challenged header is not supported yet")

        d.appendBlock()
        d.blockchain.height shouldBe 11
        val correctBlockWithStateHash = d.createBlock(ProtoBlockVersion, Nil, strictTime = true)
        correctBlockWithStateHash.header.stateHash shouldBe defined
        d.testTime.setTime(correctBlockWithStateHash.header.timestamp)
        append(correctBlockWithStateHash) shouldBe a[Right[?, ?]]

        d.rollbackTo(11)
        val invalidBlock      = d.createBlock(ProtoBlockVersion, Nil, stateHash = invalidStateHash, strictTime = true)
        val challengingBlock2 = d.createChallengingBlock(secondSigner, invalidBlock, strictTime = true)
        d.testTime.setTime(challengingBlock2.header.timestamp)
        append(challengingBlock2) shouldBe a[Right[?, ?]]
        challengingBlock2.header.challengedHeader shouldBe defined
        challengingBlock2.header.stateHash shouldBe defined
      }
    }
  }
}
