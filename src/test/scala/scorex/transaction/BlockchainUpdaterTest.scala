package scorex.transaction

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.history._
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.produce
import org.scalatest.words.ShouldVerb
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block

class BlockchainUpdaterTest extends FreeSpec with Matchers with HistoryTest with ShouldVerb with WithState {

  private val ApprovalPeriod = 100

  private val WavesSettings = DefaultWavesSettings.copy(blockchainSettings =
    DefaultWavesSettings.blockchainSettings.copy(
      functionalitySettings = DefaultWavesSettings.blockchainSettings.functionalitySettings.copy(
        featureCheckBlocksPeriod = ApprovalPeriod,
        blocksForFeatureActivation = (ApprovalPeriod * 0.9).toInt
      )
    ),
    featuresSettings = DefaultWavesSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = true)
  )

  private val WavesSettingsWithDoubling = WavesSettings.copy(blockchainSettings = WavesSettings.blockchainSettings.copy(
    functionalitySettings = WavesSettings.blockchainSettings.functionalitySettings.copy(
      doubleFeaturesPeriodsAfterHeight = 300
    )
  ))

  def appendBlock(block: Block, blockchainUpdater: BlockchainUpdater): Unit = {
    blockchainUpdater.processBlock(block)
  }

  "features approved and accepted as height grows" in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)

    h.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(3, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    }

    h.height shouldBe ApprovalPeriod
    h.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    h.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(3, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2)))
    }

    h.height shouldBe 2 * ApprovalPeriod
    h.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    h.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    h.featureStatus(3, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set()))
    }

    h.height shouldBe 3 * ApprovalPeriod
    h.featureStatus(1, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    h.featureStatus(2, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    h.featureStatus(3, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
  }

  "features rollback with block rollback" in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater
    bu.processBlock(genesisBlock)

    h.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.height shouldBe ApprovalPeriod
    h.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    h.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height shouldBe ApprovalPeriod - 1
    h.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    }

    h.height shouldBe 2 * ApprovalPeriod
    h.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    h.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height shouldBe 2 * ApprovalPeriod - 1
    h.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    h.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    bu.processBlock(getNextTestBlockWithVotes(h, Set.empty)).explicitGet()

    h.height shouldBe 2 * ApprovalPeriod
    h.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    h.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    bu.removeAfter(h.lastBlockIds(2).last).explicitGet()

    h.height shouldBe 2 * ApprovalPeriod - 1
    h.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    h.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    bu.removeAfter(h.lastBlockIds(ApprovalPeriod + 1).last).explicitGet()

    h.height shouldBe ApprovalPeriod - 1
    h.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    h.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
  }

  "feature activation height is not overriden with further periods"in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)

    h.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    h.featureActivationHeight(1) shouldBe None

    (1 until ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)

    (1 to ApprovalPeriod).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)
  }

  "feature activated only by 90% of blocks"in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)

    h.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, if (i % 2 == 0) Set(1) else Set())).explicitGet()
    }
    h.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, if (i % 10 == 0) Set() else Set(1))).explicitGet()
    }
    h.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Approved

    (1 to ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlock(h)).explicitGet()
    }
    h.featureStatus(1, ApprovalPeriod * 3) shouldBe BlockchainFeatureStatus.Activated
  }

  "features votes resets when voting window changes" in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)

    h.featureVotes(h.height) shouldBe Map.empty

    h.featureStatus(1, h.height) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
      h.featureVotes(h.height) shouldBe Map(1.toShort -> i)
    }

    h.featureStatus(1, h.height) shouldBe BlockchainFeatureStatus.Approved

    bu.processBlock(getNextTestBlockWithVotes(h, Set(1)))
    h.featureVotes(h.height) shouldBe Map(1.toShort -> 1)

    h.featureStatus(1, h.height) shouldBe BlockchainFeatureStatus.Approved
  }

  "block processing should fail if unimplemented feature was activated on blockchain when autoShutdownOnUnsupportedFeature = yes and exit with code 38" in withDomain(WavesSettings) { domain =>

    val h = domain.history
    val bu = domain.blockchainUpdater
    val signal = new Semaphore(1)
    signal.acquire()

    System.setSecurityManager(new SecurityManager {
      override def checkPermission(perm: Permission): Unit = {}

      override def checkPermission(perm: Permission, context: Object): Unit = {}

      override def checkExit(status: Int): Unit = signal.synchronized {
        super.checkExit(status)
        if (status == 38)
          signal.release()
        throw new SecurityException("System exit not allowed")
      }
    })

    bu.processBlock(genesisBlock)

    (1 to ApprovalPeriod * 2).foreach { i =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(-1))).explicitGet()
    }

    bu.processBlock(getNextTestBlockWithVotes(h, Set(-1))) should produce("ACTIVATED ON BLOCKCHAIN")

    signal.tryAcquire(10, TimeUnit.SECONDS)

    System.setSecurityManager(null)
  }

  "sunny day test when known feature activated" in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater
    bu.processBlock(genesisBlock)

    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Activated)
  }

  "empty blocks should not disable activation" in withDomain(WavesSettings) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Undefined)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }

    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Undefined)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Activated)
  }


  "doubling of feature periods works in the middle of activation period" in withDomain(WavesSettingsWithDoubling) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Activated)

    // 200 blocks passed
    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Undefined)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)

    // 300 blocks passed, the activation period should be doubled now
    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Activated)
  }

  "doubling of feature periods should work after defined height" in withDomain(WavesSettingsWithDoubling) { domain =>
    val h = domain.history
    val bu = domain.blockchainUpdater

    bu.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Undefined)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(1))).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }

    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    h.featureStatus(1, h.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Undefined)
    bu.processBlock(getNextTestBlockWithVotes(h, Set(2))).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    }
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Approved)
    bu.processBlock(getNextTestBlockWithVotes(h, Set())).explicitGet()
    h.featureStatus(2, h.height) should be(BlockchainFeatureStatus.Activated)
  }
}
