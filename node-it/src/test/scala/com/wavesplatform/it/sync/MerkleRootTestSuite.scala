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

class MerkleRootTestSuite extends BaseFreeSpec with ActivationStatusRequest with OptionValues {
  import MerkleRootTestSuite.*

  override protected def nodeConfigs: Seq[Config] = Configs

  "not able to get merkle proof before activation" in {
    miner.waitForHeight(ActivationHeight - 1)
    val txId = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None).id
    assertApiError(
      miner.getMerkleProof(txId),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
    assertApiError(
      miner.getMerkleProofPost(txId),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
  }

  "able to get merkle proof after activation" in {
    miner.waitForHeight(ActivationHeight, 2.minutes)
    val txId1 = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId2 = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId3 = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None, waitForTx = true).id
    miner.getMerkleProof(txId1, txId2, txId3).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2, txId3)
    miner.getMerkleProofPost(txId1, txId2, txId3).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2, txId3)
    miner.getMerkleProof(txId1, txId2, txId3).map(resp => resp.transactionIndex) should contain theSameElementsAs Seq(0, 1, 2)
    miner.getMerkleProofPost(txId1, txId2, txId3).map(resp => resp.transactionIndex) should contain theSameElementsAs Seq(0, 1, 2)
    miner.getMerkleProof(txId3).map(resp => resp.merkleProof.head).head shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
    miner.getMerkleProofPost(txId3).map(resp => resp.merkleProof.head).head shouldBe Base58.encode(Blake2b256.hash(Array(0.toByte)))
    assert(Base58.tryDecode(miner.getMerkleProof(txId1).head.merkleProof.head).isSuccess)
    assert(Base58.tryDecode(miner.getMerkleProofPost(txId1).head.merkleProof.head).isSuccess)
  }

  "error raised if transaction id is not valid" in {
    assertApiError(
      miner.getMerkleProof("FCymvrY43ddiKKTkznawWasoMbWd1LWyX8DUrwAAbcUA"),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )
    assertApiError(
      miner.getMerkleProofPost("FCymvrY43ddiKKTkznawWasoMbWd1LWyX8DUrwAAbcUA"),
      CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
    )

    val invalidId = "FCym43ddiKKT000kznawWasoMbWd1LWyX8DUrwAAbcUA" // id is invalid because base58 cannot contain "0"
    assertApiError(
      miner.getMerkleProof(invalidId),
      InvalidSignature
    )
    assertApiError(
      miner.getMerkleProofPost(invalidId),
      InvalidSignature
    )
  }

  "merkle proof api returns only existent txs when existent and inexistent ids passed" in {
    val txId1        = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None, waitForTx = true).id
    val txId2        = miner.broadcastTransfer(miner.keyPair, miner.address, transferAmount, minFee, None, None, waitForTx = true).id
    val inexistentTx = "FCym43ddiKKT3d4kznawWasoMbWd1LWyX8DUrwAAbcUA"
    miner.getMerkleProof(txId1, txId2, inexistentTx).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2)
    miner.getMerkleProofPost(txId1, txId2, inexistentTx).map(resp => resp.id) should contain theSameElementsAs Seq(txId1, txId2)
  }

  "merkle proof api can handle transactionsRoot changes caused by miner settings" in {

    /** In this case we check that when some of generated microblocks connected to one keyblock transfers to next keyblock due to miner setting
      * "min-micro-block-age" it causes transactionsRoot and merkleProof recalculation
      */
    val currentHeight = nodes.waitForHeightArise()

    var txIds                  = Seq[String]()
    var merkleProofGetBefore   = Seq(Seq(""))
    var merkleProofPostBefore  = Seq(Seq(""))
    var transactionsRootBefore = ""

    while (miner.height == currentHeight) {
      val newTxId                   = miner.transfer(miner.keyPair, miner.address, 1, waitForTx = true).id
      val newTxIds                  = txIds :+ newTxId
      val newMerkleProofGetBefore   = miner.getMerkleProof(newTxIds*).map(_.merkleProof)
      val newMerkleProofPostBefore  = miner.getMerkleProofPost(newTxIds*).map(_.merkleProof)
      val newTransactionsRootBefore = miner.blockAt(currentHeight).transactionsRoot.get
      if (miner.height == currentHeight) {
        txIds = newTxIds
        merkleProofGetBefore = newMerkleProofGetBefore
        merkleProofPostBefore = newMerkleProofPostBefore
        transactionsRootBefore = newTransactionsRootBefore
      }
    }
    miner.height shouldBe currentHeight + 1
    miner.getMerkleProof(txIds*).map(_.merkleProof) should not be merkleProofGetBefore
    miner.getMerkleProofPost(txIds*).map(_.merkleProof) should not be merkleProofPostBefore
    miner.blockAt(currentHeight).transactionsRoot.get should not be transactionsRootBefore
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
