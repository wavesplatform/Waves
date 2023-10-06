package com.wavesplatform.it.sync

import com.google.common.primitives.Ints
import com.typesafe.config.Config
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.it.api.SyncHttpApi.*

class LightNodeBroadcastSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((14, 1000000)))
      .withDefault(1)
      .withSpecial(2, _.lightNode)
      .buildNonConflicting()

  test("NODE-1162. Light node should correctly broadcast blocks and snapshots to the other light nodes") {
    val fullNode   = nodes.head
    val lightNode1 = nodes(1)
    val lightNode2 = nodes(2)

    fullNode.blacklist(lightNode2.networkAddress)
    lightNode2.blacklist(fullNode.networkAddress)

    (1 to 3).foreach { blockIdx =>
      val transactionIds = (1 to 50).map { idx =>
        val destPk   = Ints.toByteArray(blockIdx) ++ Ints.toByteArray(idx) ++ new Array[Byte](24)
        val destAddr = Address.fromPublicKey(PublicKey(destPk)).toString
        sender.transfer(sender.keyPair, destAddr, idx).id
      }
      transactionIds.foreach(nodes.waitForTransaction(_))
      nodes.waitForHeightArise()
    }

    val height = nodes.waitForHeightArise()

    val fullNodeState   = fullNode.debugStateAt(height - 1)
    val lightNodeState1 = lightNode1.debugStateAt(height - 1)
    val lightNodeState2 = lightNode2.debugStateAt(height - 1)

    fullNodeState.toSet shouldBe lightNodeState1.toSet
    lightNodeState1.toSet shouldBe lightNodeState2.toSet
  }
}
