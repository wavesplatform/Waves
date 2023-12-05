package com.wavesplatform.it.sync.lightnode

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.test.NumericExt

class LightNodeMiningSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.preactivatedFeatures(LightNode.id.toInt -> 2))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.light-node-block-fields-absence-interval = 2"))
      .withDefault(1)
      .withSpecial(1, _.lightNode)
      .buildNonConflicting()

  test("node can mine in light mode after light-node-block-fields-absence-interval") {
    val lightNode        = nodes.find(_.settings.enableLightMode).get
    val fullNode         = nodes.find(!_.settings.enableLightMode).get
    val lightNodeAddress = lightNode.keyPair.toAddress.toString
    val fullNodeAddress  = fullNode.keyPair.toAddress.toString

    nodes.waitForHeight(5)
    fullNode.transfer(fullNode.keyPair, lightNodeAddress, fullNode.balance(fullNodeAddress).balance - 1.waves)
    lightNode.blockSeq(2, 5).foreach(_.generator shouldBe fullNodeAddress)

    lightNode.waitForHeight(6)
    lightNode.blockAt(6).generator shouldBe lightNodeAddress
  }
}
