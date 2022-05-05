package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.Node
import com.wavesplatform.it.NodeConfigs._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.{BaseTransactionSuite, NodesFromDocker}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction

class RebroadcastTransactionSuite extends BaseTransactionSuite with NodesFromDocker {

  import RebroadcastTransactionSuite._

  override protected def nodeConfigs: Seq[Config] =
    Seq(configWithRebroadcastAllowed.withFallback(Miners.head), configWithRebroadcastAllowed.withFallback(NotMiner))

  private def nodeAIsMiner: Node    = nodes.head
  private def nodeBIsNotMiner: Node = nodes.last

  test("should rebroadcast a transaction if that's allowed in config") {
    val tx = TransferTransaction
      .selfSigned(
        2.toByte,
        nodeAIsMiner.keyPair,
        Address.fromString(nodeBIsNotMiner.address).explicitGet(),
        Waves,
        transferAmount,
        Waves,
        minFee,
        ByteStr.empty,
        System.currentTimeMillis()
      )
      .explicitGet()
      .json()

    val dockerNodeAId = docker.stopContainer(dockerNodes().head)
    val txId          = nodeBIsNotMiner.signedBroadcast(tx).id
    docker.startContainer(dockerNodeAId)
    nodeBIsNotMiner.waitForPeers(1)

    nodeAIsMiner.ensureTxDoesntExist(txId)
    nodeBIsNotMiner.signedBroadcast(tx)
    nodeAIsMiner.waitForUtxIncreased(0)
    nodeAIsMiner.utxSize shouldBe 1
  }

  test("should not rebroadcast a transaction if that's not allowed in config") {
    dockerNodes().foreach(docker.restartNode(_, configWithRebroadcastNotAllowed))

    val tx = TransferTransaction
      .selfSigned(
        2.toByte,
        nodeAIsMiner.keyPair,
        Address.fromString(nodeBIsNotMiner.address).explicitGet(),
        Waves,
        transferAmount,
        Waves,
        minFee,
        ByteStr.empty,
        System.currentTimeMillis()
      )
      .explicitGet()
      .json()

    val dockerNodeAId = docker.stopContainer(dockerNodes().head)
    val txId          = nodeBIsNotMiner.signedBroadcast(tx).id
    docker.startContainer(dockerNodeAId)
    nodeBIsNotMiner.waitForPeers(1)

    nodeAIsMiner.ensureTxDoesntExist(txId)
    nodeBIsNotMiner.signedBroadcast(tx)
    nodes.waitForHeightArise()
    nodeAIsMiner.utxSize shouldBe 0
    nodeAIsMiner.ensureTxDoesntExist(txId)
  }

  test("should not broadcast a transaction if there are not enough peers") {
    val tx = TransferTransaction
      .selfSigned(
        2.toByte,
        nodeAIsMiner.keyPair,
        Address.fromString(nodeBIsNotMiner.address).explicitGet(),
        Waves,
        transferAmount,
        Waves,
        minFee,
        ByteStr.empty,
        System.currentTimeMillis()
      )
      .explicitGet()
      .json()

    val testNode = dockerNodes().last
    try {
      docker.restartNode(testNode, configWithMinimumPeers(999))
      assertApiError(
        testNode.signedBroadcast(tx),
        CustomValidationError("There are not enough connections with peers \\(\\d+\\) to accept transaction").assertiveRegex
      )
    } finally {
      docker.restartNode(testNode, configWithMinimumPeers(0))
    }
  }
}
object RebroadcastTransactionSuite {
  private val configWithRebroadcastAllowed =
    parseString("waves.synchronization.utx-synchronizer.allow-tx-rebroadcasting = true")

  private val configWithRebroadcastNotAllowed =
    parseString("waves.synchronization.utx-synchronizer.allow-tx-rebroadcasting = false")

  private def configWithMinimumPeers(n: Int) =
    parseString(s"waves.rest-api.minimum-peers = $n")
}
