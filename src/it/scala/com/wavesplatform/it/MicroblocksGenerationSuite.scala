package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class MicroblocksGenerationSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {

  private val txsInMicroBlock = 200
  private val maxTxs = 2000

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.raw(
      s"""waves {
         |  network.enable-peers-exchange = no
         |
         |  miner {
         |    quorum = 0
         |    minimal-block-generation-offset = 60000ms
         |    micro-block-interval = 3s
         |    max-transactions-in-key-block = 0
         |    max-transactions-in-micro-block = $txsInMicroBlock
         |  }
         |
         |  blockchain.custom {
         |    functionality.pre-activated-features {
         |      2: 0
         |    }
         |
         |    store-transactions-in-state = false
         |  }
         |
         |  features.supported = [2]
         |}""".stripMargin
    ))
    .withDefault(1)
    .build()

  private val nodeAddresses = nodeConfigs.map(_.getString("address")).toSet

  private def miner = nodes.head

  s"Generate transactions and wait for one block with $maxTxs txs" in result(for {
    uploadedTxs <- processRequests(generateTransfersToRandomAddresses(maxTxs, nodeAddresses))
    _ <- miner.waitForHeight(3)
    block <- miner.blockAt(2)
  } yield {
    block.transactions.size shouldBe maxTxs

    val blockTxs = block.transactions.map(_.id)
    val diff = uploadedTxs.map(_.id).toSet -- blockTxs
    diff shouldBe empty
  }, 3.minutes)

}
