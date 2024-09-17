package com.wavesplatform.transaction

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.history
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FreeSpec
import org.scalactic.source.Position

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}
import scala.util.Try

class BlockchainUpdaterTest extends FreeSpec with HistoryTest with WithDomain {

  private val ApprovalPeriod      = 100
  private val BlocksForActivation = (ApprovalPeriod * 0.9).toInt

  private val WavesSettings = history.DefaultWavesSettings.copy(
    blockchainSettings = history.DefaultWavesSettings.blockchainSettings.copy(
      functionalitySettings = history.DefaultWavesSettings.blockchainSettings.functionalitySettings
        .copy(featureCheckBlocksPeriod = ApprovalPeriod, blocksForFeatureActivation = BlocksForActivation, preActivatedFeatures = Map.empty)
    ),
    featuresSettings = history.DefaultWavesSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = true)
  )

  private val WavesSettingsWithDoubling = WavesSettings.copy(
    blockchainSettings = WavesSettings.blockchainSettings.copy(
      functionalitySettings =
        WavesSettings.blockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map.empty, doubleFeaturesPeriodsAfterHeight = 300)
    )
  )

  def appendBlock(block: Block, blockchainUpdater: BlockchainUpdater with Blockchain): Unit = {
    blockchainUpdater.processBlock(block)
  }

  "features approved and accepted as height grows" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(3, 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1)))
    }

    b.height shouldBe ApprovalPeriod
    b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(3, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(2)))
    }

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(3, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlock(b))
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
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.height shouldBe ApprovalPeriod
    b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

    b.removeAfter(b.blockHeader(ApprovalPeriod - 1).get.id()).explicitGet()

    b.height shouldBe ApprovalPeriod - 1
    b.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    (1 to ApprovalPeriod + 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    }

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    b.removeAfter(b.blockHeader(b.height - 1).get.id()).explicitGet()

    b.height shouldBe 2 * ApprovalPeriod - 1
    b.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    b.processBlock(getNextTestBlockWithVotes(b, Seq.empty)) should beRight

    b.height shouldBe 2 * ApprovalPeriod
    b.featureStatus(1, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Activated
    b.featureStatus(2, 2 * ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

    b.removeAfter(b.blockHeader(b.height - 1).get.id()).explicitGet()

    b.height shouldBe 2 * ApprovalPeriod - 1
    b.featureStatus(1, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Approved
    b.featureStatus(2, 2 * ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined

    b.removeAfter(b.blockHeader(ApprovalPeriod - 1).get.id()).explicitGet()

    b.height shouldBe ApprovalPeriod - 1
    b.featureStatus(1, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
    b.featureStatus(2, ApprovalPeriod - 1) shouldBe BlockchainFeatureStatus.Undefined
  }

  "multiple features activation: one after another" - {
    def appendBlocks(b: BlockchainUpdaterImpl, blocks: Int, votes: Short*): Unit = (1 to blocks).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, votes)) should beRight
    }

    def check(
        b: BlockchainUpdaterImpl,
        height: Int,
        approvedFeatures: Map[Int, Int] = Map.empty,
        activatedFeatures: Map[Int, Int] = Map.empty
    )(implicit pos: Position): Unit = {
      b.height shouldBe height
      withClue("approved:") {
        b.approvedFeatures shouldBe approvedFeatures
      }
      withClue("activated:") {
        b.activatedFeatures shouldBe activatedFeatures
      }
    }

    "without votes for already activated feature" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      b.processBlock(genesisBlock)

      check(b, 1)

      markup("Approving the first feature")
      appendBlocks(b, blocks = ApprovalPeriod - 1, votes = 1)
      check(
        b,
        ApprovalPeriod,
        approvedFeatures = Map(1 -> ApprovalPeriod),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2)
      )

      markup("Approving the second feature without voting for first feature")
      appendBlocks(b, blocks = ApprovalPeriod, votes = 2)
      check(
        b,
        ApprovalPeriod * 2,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )

      markup("Activating the second feature")
      appendBlocks(b, blocks = ApprovalPeriod)
      check(
        b,
        ApprovalPeriod * 3,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )
    }

    "with votes for already activated feature" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      b.processBlock(genesisBlock)

      markup("Approving the first feature")
      appendBlocks(b, blocks = ApprovalPeriod - 1, votes = 1)

      markup("Approving the second feature, still voting for first feature")
      appendBlocks(b, blocks = ApprovalPeriod, votes = 1, 2)
      check(
        b,
        ApprovalPeriod * 2,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )

      markup("Activating the second feature")
      appendBlocks(b, blocks = ApprovalPeriod)
      check(
        b,
        ApprovalPeriod * 3,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )
    }

    "with multiple voting periods rollback" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      b.processBlock(genesisBlock)

      markup("Approving the first feature")
      appendBlocks(b, blocks = ApprovalPeriod - 1, votes = 1)

      markup("Approving the second feature")
      appendBlocks(b, blocks = ApprovalPeriod, votes = 2)

      markup("Approving the third feature")
      appendBlocks(b, blocks = ApprovalPeriod, votes = 3)

      markup("Approving the fourth feature")
      appendBlocks(b, blocks = ApprovalPeriod, votes = 4)
      check(
        b,
        ApprovalPeriod * 4,
        approvedFeatures = Map(
          1 -> ApprovalPeriod,
          2 -> ApprovalPeriod * 2,
          3 -> ApprovalPeriod * 3,
          4 -> ApprovalPeriod * 4
        ),
        activatedFeatures = Map(
          1 -> ApprovalPeriod * 2,
          2 -> ApprovalPeriod * 3,
          3 -> ApprovalPeriod * 4,
          4 -> ApprovalPeriod * 5 // Actually, it is not activated, because we haven't reach this height
        )
      )

      markup("Rollback")
      val rollbackHeight = ApprovalPeriod * 2 + 1
      b.removeAfter(b.blockHeader(rollbackHeight).get.id()).explicitGet()
      check(
        b,
        rollbackHeight,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )

      markup("Appending blocks without votes to reach the previous height")
      appendBlocks(b, blocks = ApprovalPeriod * 2 - 1)
      check(
        b,
        ApprovalPeriod * 4,
        approvedFeatures = Map(1 -> ApprovalPeriod, 2 -> ApprovalPeriod * 2),
        activatedFeatures = Map(1 -> ApprovalPeriod * 2, 2 -> ApprovalPeriod * 3)
      )
    }
  }

  "features activation after rollback and appending blocks" - {
    def appendAndRollback(b: BlockchainUpdaterImpl, rollbackToHeight: Int): Unit = {
      b.processBlock(genesisBlock)

      markup("Approving the feature")
      b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined
      (1 until ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
      }

      b.height shouldBe ApprovalPeriod
      b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

      markup("Appending blocks without votes after approval")
      (1 to 2).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      markup("Rollback before approval height")
      b.removeAfter(b.blockHeader(rollbackToHeight).get.id()).explicitGet()
      b.featureStatus(1, rollbackToHeight) shouldBe BlockchainFeatureStatus.Undefined
    }

    "when not enough votes after rollback - should not approve and activate the feature" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      appendAndRollback(b, BlocksForActivation) // 1st blocks is genesis, BlocksForActivation - 1 votes

      markup("Appending blocks to reach ApprovalPeriod height")
      (b.height until ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      b.height shouldBe ApprovalPeriod
      b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

      markup("Appending blocks to reach ApprovalPeriod * 2")
      (1 to ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      val newHeight = ApprovalPeriod * 2
      b.height shouldBe newHeight
      b.featureStatus(1, newHeight) shouldBe BlockchainFeatureStatus.Undefined
    }

    "when enough votes after rollback - should approve and activate the feature" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      appendAndRollback(b, BlocksForActivation + 1) // 1st blocks is genesis, BlocksForActivation - 1 + 1 votes

      markup("Appending blocks to reach ApprovalPeriod height")
      (b.height until ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      b.height shouldBe ApprovalPeriod
      b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

      markup("Appending blocks to reach ApprovalPeriod * 2")
      (1 to ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      val activationHeight = ApprovalPeriod * 2
      b.height shouldBe activationHeight
      b.featureStatus(1, activationHeight) shouldBe BlockchainFeatureStatus.Activated
    }
  }

  "feature activation height is not overridden with further periods" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureStatus(1, 1) shouldBe BlockchainFeatureStatus.Undefined

    b.featureActivationHeight(1) shouldBe None

    (1 until ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)

    (1 to ApprovalPeriod).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureActivationHeight(1) shouldBe Some(ApprovalPeriod * 2)
  }

  "feature activated only by 90% of blocks" - {
    "negative with insufficient votes" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      b.processBlock(genesisBlock)

      (1 until BlocksForActivation).foreach { _ =>
        b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
      }

      (BlocksForActivation to ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Undefined

      (1 to ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }

      b.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Undefined
    }

    "positive" in withDomain(WavesSettings) { domain =>
      val b = domain.blockchainUpdater
      b.processBlock(genesisBlock)

      (1 to ApprovalPeriod).foreach { i =>
        b.processBlock(getNextTestBlockWithVotes(b, if (i % 10 == 0) Seq() else Seq(1))) should beRight
      }
      b.featureStatus(1, ApprovalPeriod) shouldBe BlockchainFeatureStatus.Approved

      (1 to ApprovalPeriod).foreach { _ =>
        b.processBlock(getNextTestBlock(b)) should beRight
      }
      b.featureStatus(1, ApprovalPeriod * 2) shouldBe BlockchainFeatureStatus.Activated
    }
  }

  "features votes resets when voting window changes" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)

    b.featureVotes(b.height) shouldBe Map.empty

    b.featureStatus(1, b.height) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod).foreach { i =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1)))
      b.featureVotes(b.height) shouldBe Map(1.toShort -> i)
    }

    b.featureStatus(1, b.height) shouldBe BlockchainFeatureStatus.Approved

    b.processBlock(getNextTestBlockWithVotes(b, Seq(1)))
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
      b.processBlock(getNextTestBlockWithVotes(b, Seq(-1))) should beRight
    }

    Try(b.processBlock(getNextTestBlockWithVotes(b, Seq(-1)))).recover[Any] { case _: SecurityException => // NOP
    }

    signal.tryAcquire(10, TimeUnit.SECONDS)

    System.setSecurityManager(null)
  }

  "sunny day test when known feature activated" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater
    b.processBlock(genesisBlock)

    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "empty blocks should not disable activation" in withDomain(WavesSettings) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlock(b)) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlock(b)) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "doubling of feature periods works in the middle of activation period" in withDomain(WavesSettingsWithDoubling) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    // 200 blocks passed
    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    // 300 blocks passed, the activation period should be doubled now
    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlock(b)) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "doubling of feature periods should work after defined height" in withDomain(WavesSettingsWithDoubling) { domain =>
    val b = domain.blockchainUpdater

    b.processBlock(genesisBlock)
    // Start from 1 because of the genesis block
    (1 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(1))) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod - 1).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }

    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlock(b)) should beRight
    b.featureStatus(1, b.height) should be(BlockchainFeatureStatus.Activated)

    (0 until ApprovalPeriod * 2).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Undefined)
    b.processBlock(getNextTestBlockWithVotes(b, Seq(2))) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)

    (0 until ApprovalPeriod * 2 - 1).foreach { _ =>
      b.processBlock(getNextTestBlock(b)) should beRight
    }
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Approved)
    b.processBlock(getNextTestBlock(b)) should beRight
    b.featureStatus(2, b.height) should be(BlockchainFeatureStatus.Activated)
  }

  "correct calculate hit source in tests" in
    withDomain(RideV6) { d =>
      d.appendBlock()
      d.appendBlock()
      d.appendBlock()
      d.rollbackTo(2)
      d.appendBlock()
    }
}
