package com.wavesplatform.it.sync

import com.google.common.primitives.Ints
import com.typesafe.config.Config
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.it.api.SyncHttpApi.*

class LightNodeRollbackSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((14, 1000000)))
      .withDefault(1)
      .withSpecial(1, _.lightNode)
      .buildNonConflicting()

  test("NODE-1156. Light node should synchronize with other nodes after rollback") {
    val lightNode   = nodes.last
    val startHeight = lightNode.height

    val allTxIds = (1 to 3).flatMap { blockIdx =>
      val transactionIds = (1 to 50).map { idx =>
        val destPk   = Ints.toByteArray(blockIdx) ++ Ints.toByteArray(idx) ++ new Array[Byte](24)
        val destAddr = Address.fromPublicKey(PublicKey(destPk)).toString
        sender.transfer(sender.keyPair, destAddr, idx).id
      }
      transactionIds.foreach(lightNode.waitForTransaction(_))
      nodes.waitForHeightArise()
      transactionIds
    }

    val stateHeight        = lightNode.height
    val stateAfterFirstTry = lightNode.debugStateAt(stateHeight)

    lightNode.rollback(startHeight)
    allTxIds.foreach(lightNode.waitForTransaction(_))
    val maxHeight = sender.transactionStatus(allTxIds).flatMap(_.height).max
    sender.waitForHeight(maxHeight + 2) // so that NG fees won't affect miner's balances

    val stateAfterSecondTry = nodes.head.debugStateAt(maxHeight + 1)
    stateAfterSecondTry.toSet shouldBe stateAfterFirstTry.toSet
  }
}
