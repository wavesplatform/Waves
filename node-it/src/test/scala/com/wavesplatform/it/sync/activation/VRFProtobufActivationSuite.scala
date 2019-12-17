package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.TxVersion

class VRFProtobufActivationSuite extends BaseTransactionSuite {
  val activationHeight = 5
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id, activationHeight)))
      .buildNonConflicting()

  private val senderAcc  = pkByAddress(firstAddress)
  private val recipientAcc = pkByAddress(secondAddress)

  test("not able to broadcast txs of new versions before activation") {
    assertApiError(sender.transfer(senderAcc.stringRepr, recipientAcc.stringRepr, transferAmount, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }
    assertApiError(sender.issue(senderAcc.stringRepr, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }
    assertApiError(sender.burn(senderAcc.stringRepr, "8Yw4QmskrQauQeNjgh2fTQ4swmkNm85GTQzdHEf6QdUU", someAssetAmount, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }

  }


}
