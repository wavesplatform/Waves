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

      nodes.head.waitForHeight(ActivationHeight, 5.minutes)

      nodes.head.featureActivationStatus(BlockchainFeatures.BlockV5.id).blockchainStatus shouldBe BlockchainFeatureStatus.Activated

      val blockAtActivationHeight = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight.version shouldBe 'defined
      blockAtActivationHeight.version.value should not be Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockAtActivationHeight.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
      ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockAtActivationHeight.signature).generationSignature).get.length shouldBe Block.GenerationSignatureLength

      nodes.waitForHeightArise()

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      val blockAfterActivationHeight = nodes.head.blockAt(ActivationHeight + 1)
      val blockHeadersAfterActivationHeight = nodes.head.blockHeadersAt(ActivationHeight + 1)
      val lastBlockAfterActivationHeight = nodes.head.lastBlock
      val lastBlockHeadersAfterActivationHeight = nodes.head.lastBlockHeaders
      val blocks = Map(blockAfterActivationHeight -> blockHeadersAfterActivationHeight, lastBlockAfterActivationHeight -> lastBlockHeadersAfterActivationHeight)

      for((block, blockHeaders) <- blocks) {
        block.version shouldBe 'defined
        blockHeaders.version shouldBe 'defined
        block.version.value shouldBe Block.ProtoBlockVersion
        blockHeaders.version.value shouldBe Block.ProtoBlockVersion
        ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        ByteStr.decodeBase58(blockHeaders.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
      }
      ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockAfterActivationHeight.signature).generationSignature).get.length shouldBe Block.GenerationVRFSignatureLength
      ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockHeadersAfterActivationHeight.signature).generationSignature).get.length shouldBe Block.GenerationVRFSignatureLength

      val blockBySignatureAfterActivation = nodes.head.blockBySignature(blockAfterActivationHeight.signature)
      blockBySignatureAfterActivation.version shouldBe 'defined
      blockBySignatureAfterActivation.version.value shouldBe Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockBySignatureAfterActivation.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength

      nodes.waitForHeightArise()

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      val blockAfterVRFUsing = nodes.head.blockAt(ActivationHeight + 2)
      blockAfterVRFUsing.version shouldBe 'defined
      blockAfterVRFUsing.version.value shouldBe Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockAfterVRFUsing.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength

      val blockSeq = nodes.head.blockSeq(ActivationHeight - 1, ActivationHeight + 2)
      for (block <- blockSeq) {
        if (block.height < ActivationHeight + 1) {
          block.version shouldBe 'defined
          block.version.value should not be Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version shouldBe 'defined
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      val blockHeadersSeq = nodes.head.blockHeadersSeq(ActivationHeight - 1, ActivationHeight + 2)
      for (block <- blockHeadersSeq) {
        if (block.height < ActivationHeight + 1) {
          block.version shouldBe 'defined
          block.version.value should not be Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version shouldBe 'defined
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      val blockSeqByAddress = nodes.head.blockSeqByAddress(nodes.head.address, ActivationHeight - 1, ActivationHeight + 2)
      for (block <- blockSeqByAddress) {
        if (block.height < ActivationHeight + 1) {
          block.version shouldBe 'defined
          block.version.value should not be Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationSignatureLength
        } else {
          block.version shouldBe 'defined
          block.version.value shouldBe Block.ProtoBlockVersion
          ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        }
      }

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      nodes.waitForSameBlockHeadersAt(nodes.head.height + 5, conditionAwaitTime = 11.minutes)
    }

    "rollback to height before activation/at activation/after activation height" in {
      //rollback to height one block before activation height
      nodes.rollback(ActivationHeight - 1, returnToUTX = false)

      val blockBeforeActivationHeight1 = nodes.head.blockAt(ActivationHeight - 1)
      blockBeforeActivationHeight1.version shouldBe 'defined
      blockBeforeActivationHeight1.version.value should not be Block.ProtoBlockVersion

      nodes.head.waitForHeight(ActivationHeight, 2.minutes)
      val blockAtActivationHeight1 = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight1.version shouldBe 'defined
      blockAtActivationHeight1.version.value should not be Block.ProtoBlockVersion

      nodes.head.waitForHeight(ActivationHeight + 1, 2.minutes)
      val blockAfterActivationHeight1 = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterActivationHeight1.version shouldBe 'defined
      blockAfterActivationHeight1.version.value shouldBe Block.ProtoBlockVersion

      //rollback to activation height
      nodes.rollback(ActivationHeight, returnToUTX = false)

      val blockAtActivationHeight2 = nodes.head.blockAt(ActivationHeight)
      blockAtActivationHeight2.version shouldBe 'defined
      blockAtActivationHeight2.version.value should not be Block.ProtoBlockVersion

      nodes.head.waitForHeight(ActivationHeight + 1, 2.minutes)
      val blockAfterActivationHeight2 = nodes.head.blockAt(ActivationHeight + 1)
      blockAfterActivationHeight2.version shouldBe 'defined
      blockAfterActivationHeight2.version.value shouldBe Block.ProtoBlockVersion
      nodes.foreach(n => n.waitForHeight(ActivationHeight + 3, 2.minutes))

      //rollback to height after activation height using rollback to block with signature method
      nodes.rollbackToBlockWithSignature(nodes.head.blockAt(ActivationHeight + 1).signature)

      val blockAtActivationHeight3 = nodes.head.blockAt(ActivationHeight + 1)
      blockAtActivationHeight3.version shouldBe 'defined
      blockAtActivationHeight3.version.value shouldBe Block.ProtoBlockVersion
    }
  }
}

object BlockV5TestSuite {

  import com.wavesplatform.it.NodeConfigs.Default

  val MicroblockActivationHeight = 0
  val FairPosActivationHeight    = 0
  val ActivationHeight           = 3

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

  val Configs: Seq[Config] = Default.map(Config.withFallback(_)).take(2)
}
