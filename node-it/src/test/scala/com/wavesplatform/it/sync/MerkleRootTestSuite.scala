package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.ApiError.{CustomValidationError, InvalidSignature}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.Blake2b256
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.BaseFreeSpec
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import org.scalatest.*

import scala.concurrent.duration.*

class MerkleRootTestSuite
    extends BaseFreeSpec
    with ActivationStatusRequest
    with OptionValues {
  import MerkleRootTestSuite.*

  override protected def nodeConfigs: Seq[Config] = Configs

  "not able to get merkle proof before activation" in {
    nodes.head.waitForHeight(ActivationHeight - 1)
    val txId = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None).id
    assertApiError(
      nodes.head.getMerkleProof(txId),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
    assertApiError(
      nodes.head.getMerkleProofPost(txId),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
  }
  "able to get merkle proof after activation" in {
    nodes.head.waitForHeight(ActivationHeight, 2.minutes)
    val txId1 = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId2 = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId3 = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
    nodes.head.getMerkleProof(txId1, txId2, txId3).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2, txId3)
    nodes.head.getMerkleProofPost(txId1, txId2, txId3).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2, txId3)
    nodes.head.getMerkleProof(txId1, txId2, txId3).map(resp => resp.transactionIndex) should contain theSameElementsAs Seq(0, 1, 2)
    nodes.head.getMerkleProofPost(txId1, txId2, txId3).map(resp => resp.transactionIndex) should contain theSameElementsAs Seq(0, 1, 2)
    nodes.head.getMerkleProof(txId3).map(resp => resp.merkleProof.head).head shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
    nodes.head.getMerkleProofPost(txId3).map(resp => resp.merkleProof.head).head shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
    assert(Base58.tryDecode(nodes.head.getMerkleProof(txId1).head.merkleProof.head).isSuccess)
    assert(Base58.tryDecode(nodes.head.getMerkleProofPost(txId1).head.merkleProof.head).isSuccess)
  }
  "error raised if transaction id is not valid" in {
    assertApiError(
      nodes.head.getMerkleProof("FCymvrY43ddiKKTkznawWasoMbWd1LWyX8DUrwAAbcUA"),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
    assertApiError(
      nodes.head.getMerkleProofPost("FCymvrY43ddiKKTkznawWasoMbWd1LWyX8DUrwAAbcUA"),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )

    val invalidId = "FCym43ddiKKT000kznawWasoMbWd1LWyX8DUrwAAbcUA" //id is invalid because base58 cannot contain "0"
    assertApiError(
      nodes.head.getMerkleProof(invalidId),
      InvalidSignature
    )
    assertApiError(
      nodes.head.getMerkleProofPost(invalidId),
      InvalidSignature
    )
  }
  "merkle proof api returns only existent txs when existent and inexistent ids passed" in {
    val txId1 = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId2 = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
    val inexistentTx = "FCym43ddiKKT3d4kznawWasoMbWd1LWyX8DUrwAAbcUA"
    nodes.head.getMerkleProof(txId1, txId2, inexistentTx).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2)
    nodes.head.getMerkleProofPost(txId1, txId2, inexistentTx).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2)
  }
  "merkle proof api can handle transactionsRoot changes caused by miner settings" in {
    /**
      * In this case we check that when some of generated microblocks connected to one keyblock transfers to next keyblock
      * due to miner setting "min-micro-block-age" it causes transactionsRoot and merkleProof recalculation
    */
    nodes.waitForHeightArise()
    val currentHeight               = nodes.head.height
    val txsBuf                      = collection.mutable.ListBuffer[String]()
    var merkleProofBefore           = Seq(Seq(""))
    var merkleProofPostBefore       = Seq(Seq(""))
    var blockTransactionsRootBefore = ""
    while (nodes.head.height == currentHeight) {
      val tx = nodes.head.broadcastTransfer(nodes.head.keyPair, nodes.head.address, transferAmount, minFee, None, None, waitForTx = true).id
      if (nodes.head.height == currentHeight) {
        txsBuf += tx
        val txsSeq = txsBuf.toSeq
        merkleProofBefore = nodes.head.getMerkleProof(txsSeq*).map(resp => resp.merkleProof)
        merkleProofPostBefore = nodes.head.getMerkleProofPost(txsSeq*).map(resp => resp.merkleProof)
        blockTransactionsRootBefore = nodes.head.blockAt(currentHeight).transactionsRoot.get
      }
    }
    nodes.head.height shouldBe currentHeight + 1
    val txsSeq = txsBuf.toSeq
    nodes.head.getMerkleProof(txsSeq*).map(resp => resp.merkleProof) should not be merkleProofBefore
    nodes.head.getMerkleProofPost(txsSeq*).map(resp => resp.merkleProof) should not be merkleProofPostBefore
    nodes.head.blockAt(currentHeight).transactionsRoot.get should not be blockTransactionsRootBefore
  }
}

object MerkleRootTestSuite {

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
       |   miner {
       |      quorum = 0
       |      min-micro-block-age = 10s
       |      minimal-block-generation-offset = 30s
       |   }
       |}""".stripMargin
  )

  val Configs: Seq[Config] = Default.map(Config.withFallback(_)).take(1)
}
