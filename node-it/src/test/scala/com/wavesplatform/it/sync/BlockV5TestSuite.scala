package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}
import scorex.crypto.hash.Blake2b256

import scala.concurrent.duration._

class BlockV5TestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  "block v5 appears and blockchain grows" - {
    "when feature activation happened" in {
      nodes.head.waitForHeight(nodes.head.height + 2, 2.minute)
      val currentHeight = nodes.head.height

      val lastBlockAfterActivationHeight = nodes.head.lastBlock()
      val lastBlockHeadersAfterActivationHeight = nodes.head.lastBlockHeader()
      val blockAtActivationHeight = nodes.head.blockAt(currentHeight)
      val blockHeadersAfterActivationHeight = nodes.head.blockHeadersAt(currentHeight)
      val blockBySignatureAfterActivation = nodes.head.blockBySignature(blockAtActivationHeight.signature)
      val generationSignatureInConsensusApi = ByteStr.decodeBase58(nodes.head.blockGenerationSignature(blockAtActivationHeight.signature).generationSignature).get
      val generationSignatureInBlockJson = ByteStr.decodeBase58(blockAtActivationHeight.generationSignature.get).get

      blockAtActivationHeight.version.value shouldBe Block.ProtoBlockVersion
      blockHeadersAfterActivationHeight.version.value shouldBe Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockAtActivationHeight.vrf.value).get.length shouldBe Block.HitSourceLength
      ByteStr.decodeBase58(blockHeadersAfterActivationHeight.vrf.value).get.length shouldBe Block.HitSourceLength
      blockAtActivationHeight.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      blockHeadersAfterActivationHeight.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      generationSignatureInBlockJson shouldBe generationSignatureInConsensusApi
      generationSignatureInBlockJson.length shouldBe Block.GenerationVRFSignatureLength

      blockAtActivationHeight shouldBe blockBySignatureAfterActivation
      blockAtActivationHeight shouldBe lastBlockAfterActivationHeight
      blockHeadersAfterActivationHeight shouldBe lastBlockHeadersAfterActivationHeight
      blockAtActivationHeight.signature shouldBe blockHeadersAfterActivationHeight.signature
      blockAtActivationHeight.baseTarget shouldBe blockHeadersAfterActivationHeight.baseTarget
      blockAtActivationHeight.generationSignature shouldBe blockHeadersAfterActivationHeight.generationSignature

      //Activation height + 1
      nodes.head.waitForHeight(currentHeight + 1)

      val blockAfterVRFUsing = nodes.head.blockAt(currentHeight + 1)
      blockAfterVRFUsing.version.value shouldBe Block.ProtoBlockVersion
      ByteStr.decodeBase58(blockAfterVRFUsing.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength

      val blockSeq = nodes.head.blockSeq(currentHeight - 1, currentHeight + 1)
      for (block <- blockSeq) {
        block.version.value shouldBe Block.ProtoBlockVersion
        ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        ByteStr.decodeBase58(block.vrf.value).get.length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      val blockHeadersSeq = nodes.head.blockHeadersSeq(currentHeight - 1, currentHeight + 1)
      for (block <- blockHeadersSeq) {
        block.version.value shouldBe Block.ProtoBlockVersion
        ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        ByteStr.decodeBase58(block.vrf.value).get.length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      val blockSeqByAddress = nodes.head.blockSeqByAddress(nodes.head.address, currentHeight - 1, currentHeight + 1)
      for (block <- blockSeqByAddress) {
        block.version.value shouldBe Block.ProtoBlockVersion
        ByteStr.decodeBase58(block.generationSignature.get).get.length shouldBe Block.GenerationVRFSignatureLength
        ByteStr.decodeBase58(block.vrf.value).get.length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee, waitForTx = true)

      nodes.waitForSameBlockHeadersAt(nodes.head.height + 1, conditionAwaitTime = 5.minutes)
    }
  }
}
