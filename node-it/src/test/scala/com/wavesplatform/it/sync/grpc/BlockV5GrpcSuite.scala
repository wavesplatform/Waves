package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.api.grpc.BlockRangeRequest
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, NodeConfigs}
import org.scalatest._

import scala.concurrent.duration._

class BlockV5GrpcSuite extends freespec.AnyFreeSpec with ActivationStatusRequest with OptionValues with GrpcIntegrationSuiteWithThreeAddress {

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

      val blockV5     = sender.blockAt(currentHeight)
      val blockV5ById = sender.blockById(ByteString.copyFrom(blockV5.id().arr))

      blockV5.header.version shouldBe Block.ProtoBlockVersion
      blockV5.id().arr.length shouldBe crypto.DigestLength
      blockV5.signature.arr.length shouldBe crypto.SignatureLength
      blockV5.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockV5.signatureValid(), "transactionsRoot is not valid")
      blockV5ById.header.version shouldBe Block.ProtoBlockVersion
      blockV5ById.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockV5ById.signatureValid(), "transactionsRoot is not valid")

      sender.waitForHeight(currentHeight + 1, 2.minutes)

      val blockAfterVRFUsing     = sender.blockAt(currentHeight + 1)
      val blockAfterVRFUsingById = sender.blockById(ByteString.copyFrom(blockAfterVRFUsing.id().arr))

      blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsing.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
      ByteStr(sender.blockHeaderAt(currentHeight + 1).reference.toByteArray) shouldBe blockV5.id()
      blockAfterVRFUsingById.header.version shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsingById.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
      assert(blockAfterVRFUsingById.signatureValid(), "transactionsRoot is not valid")

      val blockSeqOfBlocksV5 = sender.blockSeq(currentHeight, currentHeight + 2)

      for (blockV5 <- blockSeqOfBlocksV5) {
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
        assert(blockV5.signatureValid(), "transactionsRoot is not valid")
      }

      val blockSeqOfBlocksV5ByAddress = sender.blockSeqByAddress(miner.address, currentHeight, currentHeight + 2)

      for (blockV5 <- blockSeqOfBlocksV5ByAddress) {
        blockV5.header.generator shouldBe miner.keyPair.publicKey
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
        assert(blockV5.signatureValid(), "transactionsRoot is not valid")
      }

      val blockSeqOfBlocksV5ByPKGrpc = NodeExtGrpc(sender).blockSeq(
        currentHeight,
        currentHeight + 2,
        BlockRangeRequest.Filter.GeneratorPublicKey(ByteString.copyFrom(miner.keyPair.publicKey.arr))
      )

      for (blockV5 <- blockSeqOfBlocksV5ByPKGrpc) {
        blockV5.header.generator shouldBe miner.keyPair.publicKey
        blockV5.header.version shouldBe Block.ProtoBlockVersion
        blockV5.header.generationSignature.arr.length shouldBe Block.GenerationVRFSignatureLength
        assert(blockV5.signatureValid(), "transactionsRoot is not valid")
      }
    }
  }
}
