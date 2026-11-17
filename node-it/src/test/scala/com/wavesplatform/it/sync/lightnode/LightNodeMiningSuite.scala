package com.wavesplatform.it.sync.lightnode

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.state.Height
import com.wavesplatform.test.NumericExt

class LightNodeMiningSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.preactivatedFeatures(LightNode.id.toInt -> Height(2)))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.light-node-block-fields-absence-interval = 2"))
      .withDefault(1)
      .withSpecial(1, _.lightNode)
      .buildNonConflicting()

  test("node can mine in light mode after light-node-block-fields-absence-interval") {
    val lightNode        = nodes.find(_.settings.enableLightMode).get
    val fullNode         = nodes.find(!_.settings.enableLightMode).get
    val lightNodeAddress = lightNode.keyPair.toAddress.toString
    val fullNodeAddress  = fullNode.keyPair.toAddress.toString

    nodes.waitForHeight(Height(5))
    // The light node schedules mining once the block fields are supported at the current height (4), so it may already compete for block 5
    lightNode.blockSeq(Height(2), Height(4)).foreach(_.generator shouldBe fullNodeAddress)

    // After the transfer the full node cannot generate, so the next block is the light node's
    val transfer       = fullNode.transfer(fullNode.keyPair, lightNodeAddress, fullNode.balance(fullNodeAddress).balance - 1.waves)
    val transferHeight = Height(fullNode.waitForTransaction(transfer.id).height)

    lightNode.waitForHeight(transferHeight + 1)
    lightNode.blockAt(transferHeight + 1).generator shouldBe lightNodeAddress
  }
}
