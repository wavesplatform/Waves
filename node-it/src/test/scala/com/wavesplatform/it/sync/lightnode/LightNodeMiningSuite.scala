package com.wavesplatform.it.sync.lightnode

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}

class LightNodeMiningSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.lightNode)
      .withDefault(2)
      .buildNonConflicting()

  test("nodes can mine in light mode") {
    val first  = nodes.head
    val second = nodes.last

    val tx1 = first.transfer(first.keyPair, second.address, 1, waitForTx = true)
    nodes.waitForHeightArise()
    second.transactionStatus(tx1.id).applicationStatus.get shouldBe "succeeded"

    val tx2 = second.transfer(second.keyPair, first.address, 1, waitForTx = true)
    nodes.waitForHeightArise()
    first.transactionStatus(tx2.id).applicationStatus.get shouldBe "succeeded"
  }
}
