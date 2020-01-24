package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class BlockV5TestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {
  import BlockV5TestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  "block v5 appears and blockchain grows" - {
    "when feature activation happened" in {

      nodes.head.featureActivationStatus(BlockchainFeatures.BlockV5.id).blockchainStatus should not be BlockchainFeatureStatus.Activated

      //Activation height - 1
      nodes.head.waitForHeight(ActivationHeight - 1, 5.minutes)

      val blockAtActivationHeight = nodes.head.blockAt(ActivationHeight - 1)
      val blockHeadersAtActivationHeight = nodes.head.blockHeadersAt(ActivationHeight - 1)
      blockAtActivationHeight.version.value shouldBe Block.RewardBlockVersion
      ByteStr.decodeBase58(blockAtActivationHeight.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
      ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockAtActivationHeight.signature).generationSignature).get.length shouldBe Block.GenerationSignatureLength
      blockAtActivationHeight.baseTarget shouldBe blockHeadersAtActivationHeight.baseTarget

      //Activation height
      nodes.head.waitForHeight(ActivationHeight)

      val lastBlockAfterActivationHeight = nodes.head.lastBlock
      val lastBlockHeadersAfterActivationHeight = nodes.head.lastBlockHeader
      val blockAfterActivationHeight = nodes.head.blockAt(ActivationHeight)
      val blockHeadersAfterActivationHeight = nodes.head.blockHeadersAt(ActivationHeight)
      val blockBySignatureAfterActivation = nodes.head.blockBySignature(blockAfterActivationHeight.signature)
      val generationSignatureInConsensusApi = ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockAfterActivationHeight.signature).generationSignature).get
      val generationSignatureInBlockJson = ByteStr.decodeBase58(blockAfterActivationHeight.generationSignature.get).get

      blockAfterActivationHeight.version.value shouldBe Block.ProtoBlockVersion
      blockHeadersAfterActivationHeight.version.value shouldBe Block.ProtoBlockVersion
      generationSignatureInBlockJson shouldBe generationSignatureInConsensusApi
      generationSignatureInBlockJson.length shouldBe Block.GenerationVRFSignatureLength

      blockAfterActivationHeight shouldBe blockBySignatureAfterActivation
      blockAfterActivationHeight shouldBe lastBlockAfterActivationHeight
      blockHeadersAfterActivationHeight shouldBe lastBlockHeadersAfterActivationHeight
      blockAfterActivationHeight.transactionsRoot shouldBe 'defined
      blockAfterActivationHeight.transactionsRoot shouldBe blockHeadersAfterActivationHeight.transactionsRoot
      blockAfterActivationHeight.signature shouldBe blockHeadersAfterActivationHeight.signature
      blockAfterActivationHeight.baseTarget shouldBe blockHeadersAfterActivationHeight.baseTarget
      blockAfterActivationHeight.generationSignature shouldBe blockHeadersAfterActivationHeight.generationSignature

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      //Activation height + 2
      nodes.head.waitForHeight(ActivationHeight + 1)

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      val blockAfterVRFUsing = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterVRFUsing.version.value shouldBe Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockAfterVRFUsing.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength

      val blockSeq = nodes.head.blockSeq(ActivationHeight - 1, ActivationHeight + 1)
      for (block <- blockSeq) {
        if (block.height < ActivationHeight) {
          block.version.value shouldBe Block.RewardBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      val blockHeadersSeq = nodes.head.blockHeadersSeq(ActivationHeight - 1, ActivationHeight + 1)
      for (block <- blockHeadersSeq) {
        if (block.height < ActivationHeight) {
          block.version.value shouldBe Block.RewardBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      val blockSeqByAddress = nodes.head.blockSeqByAddress(nodes.head.address, ActivationHeight - 1, ActivationHeight + 1)
      for (block <- blockSeqByAddress) {
        if (block.height < ActivationHeight) {
          block.version.value shouldBe Block.RewardBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      nodes.waitForSameBlockHeadersAt(nodes.head.height + 1, conditionAwaitTime = 5.minutes)
    }

    "rollback to height before activation/at activation/after activation height" in {
      //rollback to height one block before activation height
      nodes.rollback(ActivationHeight - 1, returnToUTX = true)

      val blockBeforeActivationHeight1 = nodes.head.blockAt(ActivationHeight - 1)
      blockBeforeActivationHeight1.version.value shouldBe Block.RewardBlockVersion
      val returnedTxIds = nodes.head.utx.map(tx => tx.id)

      nodes.head.waitForHeight(ActivationHeight, 2.minutes)
      val blockAtActivationHeight1 = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight1.version.value shouldBe Block.ProtoBlockVersion

      nodes.head.waitForHeight(ActivationHeight + 1, 2.minutes)
      val blockAfterActivationHeight1 = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterActivationHeight1.version.value shouldBe Block.ProtoBlockVersion
      nodes.waitForHeightArise()

      returnedTxIds.foreach(nodes.head.waitForTransaction(_))

      //rollback to activation height
      nodes.rollback(ActivationHeight, returnToUTX = false)

      val blockAtActivationHeight2 = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight2.version.value shouldBe Block.ProtoBlockVersion

      nodes.head.waitForHeight(ActivationHeight + 1, 2.minutes)
      val blockAfterActivationHeight2 = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterActivationHeight2.version.value shouldBe Block.ProtoBlockVersion
      nodes.waitForHeightArise()

      //rollback to height after activation height using rollback to block with signature method
      nodes.rollbackToBlockWithSignature(nodes.head.blockAt(ActivationHeight + 1).signature)

      val blockAtActivationHeight3 = nodes.head.blockAt(ActivationHeight + 1)
      blockAtActivationHeight3.version.value shouldBe Block.ProtoBlockVersion
    }
  }
}

object BlockV5TestSuite {

  import com.wavesplatform.it.NodeConfigs.Default

  val MicroblockActivationHeight = 0
  val FairPosActivationHeight    = 0
  val ActivationHeight           = 4

  val Config: Config = ConfigFactory.parseString(
    s"""
       |waves {
       |   blockchain.custom {
       |      functionality {
       |        pre-activated-features {
       |          ${BlockchainFeatures.NG.id} = $MicroblockActivationHeight,
       |          ${BlockchainFeatures.FairPoS.id} = $FairPosActivationHeight,
       |          ${BlockchainFeatures.BlockV5.id} = $ActivationHeight
       |        }
       |        generation-balance-depth-from-50-to-1000-after-height = 1000
       |      }
       |   }
       |   miner.quorum = 1
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Default.map(Config.withFallback(_)).tail.take(2)
}
