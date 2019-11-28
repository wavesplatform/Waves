package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.block.Block
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, ReportingTestName}
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

class BlockV5GrpcSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues
    with GrpcIntegrationSuiteWithThreeAddress {

  import com.wavesplatform.it.sync.BlockV5TestSuite._
  override protected def nodeConfigs: Seq[Config] = Configs

  "block v5 appears and blockchain grows" - {
    "when feature activation happened" in {
      sender.grpc.waitForHeight(ActivationHeight)

      val blockAtActivationHeight = sender.grpc.blockAt(ActivationHeight)
      val blockAtActivationHeightById = sender.grpc.blockById(ByteString.copyFrom(blockAtActivationHeight.signature))

      blockAtActivationHeight.header.version should not be Block.ProtoBlockVersion
      blockAtActivationHeight.header.generationSignature.length shouldBe Block.GenerationSignatureLength
      blockAtActivationHeightById.header.version should not be Block.ProtoBlockVersion
      blockAtActivationHeightById.header.generationSignature.length shouldBe Block.GenerationSignatureLength

      sender.grpc.waitForHeight(ActivationHeight + 1)

      val blockAfterActivationHeight = sender.grpc.blockAt(ActivationHeight + 1)
      val blockAfterActivationHeightById = sender.grpc.blockById(ByteString.copyFrom(blockAfterActivationHeight.signature))

      blockAfterActivationHeight.header.version shouldBe Block.ProtoBlockVersion
      blockAfterActivationHeight.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      blockAfterActivationHeightById.header.version shouldBe Block.ProtoBlockVersion
      blockAfterActivationHeightById.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength

      sender.grpc.waitForHeight(ActivationHeight + 2)

      val blockAfterVRFUsing = sender.grpc.blockAt(ActivationHeight + 2)
      val blockAfterVRFUsingById = sender.grpc.blockById(ByteString.copyFrom(blockAfterVRFUsing.signature))

      blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsing.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      blockAfterVRFUsingById.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsingById.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength

      val blockSeqOfBlocksV3 = sender.grpc.blockSeq(ActivationHeight - 1, ActivationHeight)
      val blockSeqOfBlocksV5 = sender.grpc.blockSeq(ActivationHeight + 1, ActivationHeight + 2)

      for (blockV3 <- blockSeqOfBlocksV3) {
        blockV3.header.version should not be Block.ProtoBlockVersion
        blockV3.header.generationSignature.length shouldBe Block.GenerationSignatureLength
      }

      for (blockV5 <- blockSeqOfBlocksV5) {
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      }

      val blockSeqOfBlocksV3ByAddress = sender.grpc.blockSeqByAddress(miner.address, ActivationHeight - 1, ActivationHeight)
      val blockSeqOfBlocksV5ByAddress = sender.grpc.blockSeqByAddress(miner.address, ActivationHeight + 1, ActivationHeight + 2)

      for (blockV3 <- blockSeqOfBlocksV3ByAddress) {
        blockV3.header.version should not be Block.ProtoBlockVersion
        blockV3.header.generationSignature.length shouldBe Block.GenerationSignatureLength
      }

      for (blockV5 <- blockSeqOfBlocksV5ByAddress) {
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      }
    }
  }
}
