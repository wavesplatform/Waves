package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.Address
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.NodeConfigs._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransactionV2

class RebroadcastTransactionSuite extends BaseTransactionSuite with NodesFromDocker {

  import RebroadcastTransactionSuite._

  override protected def nodeConfigs: Seq[Config] =
    Seq(configWithRebroadcastAllowed.withFallback(NotMiner), configWithRebroadcastAllowed.withFallback(Miners.head))

  private def nodeA: Node = nodes.head
  private def nodeB: Node = nodes.last

  test("should rebroadcast a transaction if that's allowed in config") {
    val tx = TransferTransactionV2
      .selfSigned(Waves,
                  nodeA.privateKey,
                  Address.fromString(nodeB.address).right.get,
                  transferAmount,
                  System.currentTimeMillis(),
                  Waves,
                  minFee,
                  Array.emptyByteArray)
      .explicitGet()
      .json()

    val dockerNodeBId = docker.stopContainer(dockerNodes.apply().last)
    val txId          = nodeA.signedBroadcast(tx).id
    docker.startContainer(dockerNodeBId)

    nodeB.ensureTxDoesntExist(txId)
    nodeA.signedBroadcast(tx)
    nodeB.waitForUtxIncreased(0)
    nodeB.utxSize shouldBe 1

  }
  test("should not rebroadcast a transaction if that's not allowed in config") {
    dockerNodes().foreach(docker.restartNode(_, configWithRebroadcastNotAllowed))
    val tx = TransferTransactionV2
      .selfSigned(Waves,
                  nodeA.privateKey,
                  Address.fromString(nodeB.address).right.get,
                  transferAmount,
                  System.currentTimeMillis(),
                  Waves,
                  minFee,
                  Array.emptyByteArray)
      .explicitGet()
      .json()

    val dockerNodeBId = docker.stopContainer(dockerNodes.apply().last)
    val txId          = nodeA.signedBroadcast(tx).id
    docker.startContainer(dockerNodeBId)

    nodeB.ensureTxDoesntExist(txId)
    nodeA.signedBroadcast(tx)
    nodes.waitForHeightArise()
    nodeB.utxSize shouldBe 0
    nodeB.ensureTxDoesntExist(txId)

  }
}
object RebroadcastTransactionSuite {

  private val configWithRebroadcastAllowed =
    parseString(s"""
                   |waves {
                   | rest-api {
                   |    allow-tx-rebroadcasting = true
                   |  }
                   |}""".stripMargin)

  private val configWithRebroadcastNotAllowed =
    parseString(s"""
                   |waves {
                   | rest-api {
                   |    allow-tx-rebroadcasting = false
                   |  }
                   |}""".stripMargin)

}
