package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.block.Block
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, NodeConfigs, ReportingTestName}
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class BlockV5GrpcSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues
    with GrpcIntegrationSuiteWithThreeAddress {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  "block v5 appears and blockchain grows" - {
    "when feature activation happened" in {
      sender.waitForHeight(sender.height + 1, 2.minutes)
      val currentHeight = sender.height

      val blockV5 = sender.blockAt(currentHeight)
      val blockV5ById = sender.blockById(ByteString.copyFrom(blockV5.uniqueId))

      blockV5.header.version shouldBe Block.ProtoBlockVersion
      blockV5.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockV5.transactionsRootValid(), "transactionsRoot is not valid")
      blockV5ById.header.version shouldBe Block.ProtoBlockVersion
      blockV5ById.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockV5ById.transactionsRootValid(), "transactionsRoot is not valid")

      sender.waitForHeight(currentHeight + 1, 2.minutes)

      val blockAfterVRFUsing = sender.blockAt(currentHeight + 1)
      val blockAfterVRFUsingById = sender.blockById(ByteString.copyFrom(blockAfterVRFUsing.uniqueId))

      blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsing.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      blockAfterVRFUsingById.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsingById.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockAfterVRFUsingById.transactionsRootValid(), "transactionsRoot is not valid")

      val blockSeqOfBlocksV5 = sender.blockSeq(currentHeight, currentHeight + 2)

      for (blockV5 <- blockSeqOfBlocksV5) {
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
        assert(blockV5.transactionsRootValid(), "transactionsRoot is not valid")
      }

      val blockSeqOfBlocksV5ByAddress = sender.blockSeqByAddress(miner.address, currentHeight, currentHeight + 2)

      for (blockV5 <- blockSeqOfBlocksV5ByAddress) {
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.length shouldBe Block.GenerationVRFSignatureLength
        assert(blockV5.transactionsRootValid(), "transactionsRoot is not valid")
      }
    }
  }
}
