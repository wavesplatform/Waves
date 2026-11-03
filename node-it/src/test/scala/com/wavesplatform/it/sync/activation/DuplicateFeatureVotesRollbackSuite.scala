package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.consensus.NxtPoSCalculator
import com.wavesplatform.consensus.PoSCalculator.hit
import com.wavesplatform.crypto
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs, ReportingTestName}
import com.wavesplatform.network.RawBytes
import com.wavesplatform.state.Height
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/** The miner refuses to forge a block with duplicate feature votes, so the test forges all blocks itself and sends them to the node by network.
  * Before Finality Improvements (27) activates the header validation such blocks are accepted, and every duplicate is counted as a separate vote.
  */
class DuplicateFeatureVotesRollbackSuite extends BaseFreeSpec with ActivationStatusRequest with ReportingTestName {
  private val votingInterval      = 4
  private val blocksForActivation = votingInterval // Unreachable with one vote per block: the genesis block at height 1 doesn't vote
  private val feature             = BlockchainFeatures.SmallerMinimalGeneratingBalance
  private val featureNum: Short   = feature.id
  private val duplicateVotes      = Seq.fill(blocksForActivation)(featureNum)

  private val approvalHeight   = Height(votingInterval)
  private val activationHeight = Height(votingInterval * 2)

  private val genesisHeight = Height(1)
  private val votingHeight  = Height(2) // The only block with feature votes

  import NodeConfigs.*

  // The account with the largest genesis balance, so the forged blocks have short delays
  private val signer: KeyPair = KeyPair.fromSeed(NotMiner.getString("account-seed")).explicitGet()
  private val signerBalance   = 6_000_000_000_000_000L // From template.conf, never decreases: blocks have no transactions, only rewards

  override protected def nodeConfigs: Seq[Config] =
    Seq(BiggestMiner.overrides(s"""waves {
                                  |  blockchain.custom.functionality {
                                  |    feature-check-blocks-period = $votingInterval
                                  |    blocks-for-feature-activation = $blocksForActivation
                                  |  }
                                  |  miner.enable = no
                                  |}""".stripMargin))

  private def node = nodes.head

  private def featureVotesAt(h: Height): Seq[Short] =
    (Json.parse(node.get(s"/blocks/headers/at/$h").getResponseBody) \ "features").asOpt[Seq[Short]].getOrElse(Seq.empty)

  /** Mirrors the checks in PoSSelector: FairPoS is not activated, so NxtPoSCalculator is used, and BlockV5 is pre-activated, so blocks have VRF */
  private def forgeBlock(featureVotes: Seq[Short]): Block = {
    val parentHeight = node.height
    val parent       = node.blockHeaderAt(parentHeight)
    val greatGrandParentTs =
      Option.when(parentHeight.toInt > 2)(node.blockHeaderAt(Height(parentHeight.toInt - 2)).timestamp)

    val parentHitSource  = ByteStr.decodeBase58(parent.vrf.orElse(parent.generationSignature).get).get
    val parentBaseTarget = parent.baseTarget.get.toLong

    val generationSignature = crypto.signVRF(signer.privateKey, parentHitSource.arr)
    val hitSource           = crypto.verifyVRF(generationSignature, parentHitSource.arr, signer.publicKey).explicitGet()
    val minTimestamp        = parent.timestamp + NxtPoSCalculator.calculateDelay(hit(hitSource.arr), parentBaseTarget, signerBalance)
    val timestamp           = math.max(minTimestamp, System.currentTimeMillis())

    val baseTarget = NxtPoSCalculator.calculateBaseTarget(
      node.settings.blockchainSettings.genesisSettings.averageBlockDelay.toSeconds,
      parentHeight.toInt,
      parentBaseTarget,
      parent.timestamp,
      greatGrandParentTs,
      timestamp
    )

    // Block.buildAndSign validates the header and rejects duplicate votes
    val unsigned = Block
      .create(
        version = Block.ProtoBlockVersion,
        timestamp = timestamp,
        reference = ByteStr.decodeBase58(parent.id).get,
        baseTarget = baseTarget,
        generationSignature = generationSignature,
        generator = signer.publicKey,
        featureVotes = featureVotes,
        rewardVote = -1L,
        transactionData = Nil,
        stateHash = None,
        challengedHeader = None,
        finalizationVoting = None
      )
    unsigned.copy(signature = crypto.sign(signer.privateKey, unsigned.bodyBytes()))
  }

  private def appendBlock(featureVotes: Seq[Short] = Nil): Block = {
    val expectedHeight = node.height.next
    val block          = forgeBlock(featureVotes)

    val waitMs = block.header.timestamp - System.currentTimeMillis()
    if (waitMs > 0) Thread.sleep(waitMs)

    Await.result(node.sendByNetwork(RawBytes.fromBlock(block)), 30.seconds)
    node.waitForHeight(expectedHeight)
    withClue(s"block at $expectedHeight: ") {
      node.blockHeaderAt(expectedHeight).id shouldBe block.id().toString
    }
    block
  }

  private def appendBlocksUntil(h: Height): Unit =
    while (node.height.toInt < h.toInt) appendBlock()

  private def assertNotVoted(): Unit = {
    val status = node.featureActivationStatus(featureNum)
    status.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
    status.activationHeight shouldBe None
    status.supportingBlocks should contain(0)
  }

  "block with duplicate feature votes is accepted" in {
    node.height shouldBe genesisHeight

    appendBlock(duplicateVotes)
    featureVotesAt(votingHeight) shouldBe duplicateVotes
    // Votes of a liquid block are counted once
    node.featureActivationStatus(featureNum).supportingBlocks should contain(1)

    appendBlock() // Persists the block with votes
    node.featureActivationStatus(featureNum).supportingBlocks should contain(duplicateVotes.size)
  }

  "feature is approved and activated by a single block" in {
    appendBlocksUntil(approvalHeight)
    assertApprovedStatus(node.featureActivationStatus(featureNum), activationHeight.toInt, NodeFeatureStatus.Implemented)

    appendBlocksUntil(activationHeight)
    assertActivatedStatus(node.featureActivationStatus(featureNum), activationHeight.toInt, NodeFeatureStatus.Implemented)

    appendBlock() // Persists the block at the activation height
  }

  "rollback to the genesis block discards the votes, the approval and the activation" in {
    val genesisId = node.blockHeaderAt(genesisHeight).id

    node.rollback(genesisHeight, returnToUTX = false)

    node.height shouldBe genesisHeight
    node.lastBlockHeader().id shouldBe genesisId
    assertNotVoted()
  }

  "feature is not approved again without the discarded votes" in {
    appendBlocksUntil(approvalHeight)
    featureVotesAt(votingHeight) shouldBe empty
    assertNotVoted()
  }
}
