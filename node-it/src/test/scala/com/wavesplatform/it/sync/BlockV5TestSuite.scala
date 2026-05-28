package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.crypto.Blake2b256
import com.wavesplatform.it.BaseFreeSpec
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.state.Height
import org.scalatest.*

import scala.concurrent.duration.*

class BlockV5TestSuite extends BaseFreeSpec with ActivationStatusRequest with OptionValues {
  import com.wavesplatform.it.NodeConfigs.*
  override val nodeConfigs: Seq[Config] = Seq(
    Miners(3).quorum(0),
    Default.head.notMiner
  )

  var currentHeight = Height(0)

  "block v5 appears and blockchain grows" - {
    "check block v5 at current height" in {
      nodes.head.waitForHeight(nodes.head.height + 2, 2.minute)
      currentHeight = nodes.head.height

      val lastBlockCurrentHeight         = nodes.head.lastBlock()
      val lastBlockHeaderCurrentHeight   = nodes.head.lastBlockHeader()
      val blockAtCurrentHeight           = nodes.head.blockAt(currentHeight)
      val blockHeaderCurrentHeight       = nodes.head.blockHeaderAt(currentHeight)
      val blockBySignatureCurrentHeight  = nodes.head.blockById(blockAtCurrentHeight.id)
      val generationSignatureInBlockJson = ByteStr.decodeBase58(blockAtCurrentHeight.generationSignature.get).get

      blockAtCurrentHeight.version.value shouldBe Block.ProtoBlockVersion
      Base58.decode(blockAtCurrentHeight.id).length shouldBe crypto.DigestLength
      blockHeaderCurrentHeight.version.value shouldBe Block.ProtoBlockVersion
      Base58.decode(blockAtCurrentHeight.vrf.value).length shouldBe Block.HitSourceLength
      Base58.decode(blockHeaderCurrentHeight.vrf.value).length shouldBe Block.HitSourceLength
      blockAtCurrentHeight.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      blockHeaderCurrentHeight.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      generationSignatureInBlockJson.arr.length shouldBe Block.GenerationVRFSignatureLength

      blockAtCurrentHeight shouldBe blockBySignatureCurrentHeight
      blockAtCurrentHeight shouldBe lastBlockCurrentHeight
      blockHeaderCurrentHeight shouldBe lastBlockHeaderCurrentHeight
      blockAtCurrentHeight.signature shouldBe blockHeaderCurrentHeight.signature
      blockAtCurrentHeight.baseTarget shouldBe blockHeaderCurrentHeight.baseTarget
      blockAtCurrentHeight.generationSignature shouldBe blockHeaderCurrentHeight.generationSignature
    }

    "check block v5 at next height" in {
      // Activation height + 1
      nodes.head.waitForHeight(currentHeight + 1)

      val blockAfterVRFUsing = nodes.head.blockAt(currentHeight + 1)
      blockAfterVRFUsing.version.value shouldBe Block.ProtoBlockVersion
      blockAfterVRFUsing.reference shouldBe nodes.head.blockAt(currentHeight).id
      Base58.decode(blockAfterVRFUsing.generationSignature.get).length shouldBe Block.GenerationVRFSignatureLength

      val blockSeq = nodes.head.blockSeq(currentHeight - 1, currentHeight + 1)
      for (block <- blockSeq) {
        block.version.value shouldBe Block.ProtoBlockVersion
        Base58.decode(block.generationSignature.get).length shouldBe Block.GenerationVRFSignatureLength
        Base58.decode(block.vrf.value).length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      val blockHeadersSeq = nodes.head.blockHeadersSeq(currentHeight - 1, currentHeight + 1)
      for (block <- blockHeadersSeq) {
        block.version.value shouldBe Block.ProtoBlockVersion
        Base58.decode(block.generationSignature.get).length shouldBe Block.GenerationVRFSignatureLength
        Base58.decode(block.vrf.value).length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      val blockSeqByAddress = nodes.head.blockSeqByAddress(nodes.head.address, currentHeight - 1, currentHeight + 1)
      for (block <- blockSeqByAddress) {
        block.version.value shouldBe Block.ProtoBlockVersion
        Base58.decode(block.generationSignature.get).length shouldBe Block.GenerationVRFSignatureLength
        Base58.decode(block.vrf.value).length shouldBe Block.HitSourceLength
        block.transactionsRoot.value shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
      }

      nodes.head.transfer(nodes.head.keyPair, nodes.last.address, transferAmount, minFee, waitForTx = true)

      nodes.waitForSameBlockHeadersAt(nodes.head.height + 1, conditionAwaitTime = 5.minutes)
    }
  }
}
