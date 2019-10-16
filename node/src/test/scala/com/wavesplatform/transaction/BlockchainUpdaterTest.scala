package com.wavesplatform.transaction

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.history
import com.wavesplatform.state._
import org.scalatest.words.ShouldVerb
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Try

class BlockchainUpdaterTest extends FreeSpec with Matchers with HistoryTest with ShouldVerb with WithDomain {

  private val ApprovalPeriod = 100

  private val WavesSettings = history.DefaultWavesSettings.copy(
    blockchainSettings = history.DefaultWavesSettings.blockchainSettings.copy(
      functionalitySettings = history.DefaultWavesSettings.blockchainSettings.functionalitySettings.copy(
        featureCheckBlocksPeriod = ApprovalPeriod,
        blocksForFeatureActivation = (ApprovalPeriod * 0.9).toInt,
        preActivatedFeatures = Map.empty
      )
    ),
    featuresSettings = history.DefaultWavesSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = true)
  )

  private val WavesSettingsWithDoubling = WavesSettings.copy(
    blockchainSettings = WavesSettings.blockchainSettings.copy(
      functionalitySettings = WavesSettings.blockchainSettings.functionalitySettings.copy(
        doubleFeaturesPeriodsAfterHeight = 300,
        preActivatedFeatures = Map.empty
      )
    )
  )

  def appendBlock(block: Block, blockchainUpdater: BlockchainUpdater): Unit = {
    blockchainUpdater.processBlock(block)
  }

  "features approved and accepted as height grows" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(3, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1)))
    }

    b.height shouldBe ApprovalPeriod
    b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(3, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(2)))
    }

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(3, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set()))
    }

    b.height shouldBe 3 * ApprovalPeriod
    b.featureStatus(1, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(3, 3 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
  }

  "features rollback with block rollback" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater
    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.height shouldBe ApprovalPeriod
    b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    b.removeAfter(b.lastBlockIds(2).last).explicitGet()

    b.height shouldBe ApprovalPeriod - 1
    b.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    }

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    b.removeAfter(b.lastBlockIds(2).last).explicitGet()

    b.height shouldBe 2 * ApprovalPeriod - 1
    b.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    b.processBlock(getNextTestBlockWithVotes(b, Set.empty)).explicitGet()

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    b.removeAfter(b.lastBlockIds(2).last).explicitGet()

    b.height shouldBe 2 * ApprovalPeriod - 1
    b.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    b.removeAfter(b.lastBlockIds(ApprovalPeriod + 1).last).explicitGet()

    b.height shouldBe ApprovalPeriod - 1
    b.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
  }

  "feature activation height is not overriden with further periods" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    b.featureActivationHeight(1) shouldBe None

    (1 until ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)
  }

  "feature activated only by 90% of blocks" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      b.processBlock(getNextTestBlockWithVotes(b, if (i % 2 == 0) Set(1) else Set())).explicitGet()
    }
    b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { i =>
      b.processBlock(getNextTestBlockWithVotes(b, if (i % 10 == 0) Set() else Set(1))).explicitGet()
    }
    b.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Approved

    (1 to ApprovalPeriod).foreach { i =>
      b.processBlock(getNextTestBlock(b)).explicitGet()
    }
    b.featureStatus(1, ApprovalPeriod * 3) shouldBe BlockchainFeatureStatus.Activated
  }

  "features votes resets when voting window changes" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureVotes(b.height) shouldBe Map.empty

    b.featureStatus(1, b.height) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1)))
      b.featureVotes(b.height) shouldBe Map(1.toShort -> i)
    }

    b.featureStatus(1, b.height) shouldBe BlockchainFeatureStatus.Approved

    b.processBlock(getNextTestBlockWithVotes(b, Set(1)))
    b.featureVotes(b.height) shouldBe Map(1.toShort -> 1)

    b.featureStatus(1, b.height) shouldBe BlockchainFeatureStatus.Approved
  }

  "block processing should fail if unimplemented feature was activated on blockchain when autoShutdownOnUnsupportedFeature = yes and exit with code 38" in withDomain(
    WavesSettings
  ) { domain =>
    val b      = domain.blockchainUpdater
    val signal = new Semaphore(1)
    signal.acquire()

    System.setSecurityManager(new SecurityManager {
      override def checkPermission(perm: Permission): Unit = {}

      override def checkPermission(perm: Permission, context: Object): Unit = {}

      override def checkExit(status: Int): Unit = signal.synchronized {
        super.checkExit(status)
        if (status == 38)
          signal.release()
        throw new SecurityException("System exit is not allowed")
      }
    })

    b.processBlock(genesisBlock)

    (1 to ApprovalPeriod * 2 - 2).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(-1))).explicitGet()
    }

    Try(b.processBlock(getNextTestBlockWithVotes(b, Set(-1)))).recover {
      case _: SecurityException => // NOP
    }

    signal.tryAcquire(10, TimeUnit.SECONDS)

    System.setSecurityManager(null)
  }

  "sunny day test when known feature activated" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater
    b.processBlock(genesisBlock)

    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "empty blocks should not disable activation" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "doubling of feature periods works in the middle of activation period" in withDomain(WavesSettingsWithDoubling) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    // 200 blocks passed
    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    // 300 blocks passed, the activation period should be doubled now
    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "doubling of feature periods should work after defined height" in withDomain(WavesSettingsWithDoubling) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Set(1))).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Set(2))).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Set())).explicitGet()
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }
}
